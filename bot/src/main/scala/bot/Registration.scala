package bot

import bot.chaster.{AccessToken, ChasterClient, LockStatus}
import bot.db.{Filter, RegisteredUserRepository, User, UserTokenRepository}
import bot.db.Filters.*
import bot.model.DiscordID.given_Put_DiscordID
import bot.model.{ChasterID, DiscordID, Guild, Member, Message, RegisteredUser, User, UserToken}
import cats.effect.{IO, Ref}
import doobie.syntax.connectionio.*
import io.circe.Decoder
import org.http4s.*
import org.http4s.dsl.io.*
import org.http4s.circe.CirceEntityDecoder.*
import org.http4s.Method.*
import org.http4s.client.dsl.io.*
import org.http4s.headers.{Authorization, Location}
import cats.syntax.option.*
import cats.syntax.applicative.*
import cats.syntax.traverse.*
import doobie.syntax.string.*
import doobie.{Get, Put}

import java.time.Instant
import scala.concurrent.duration.*
import java.util.UUID
import scala.util.Try
import bot.syntax.io.*
import cats.data.{EitherT, OptionT}
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger

class Registration private(
  registeredUserRepository: RegisteredUserRepository,
  userTokenRepository: UserTokenRepository,
  chasterClient: ChasterClient,
  registrations: Ref[IO, Map[UUID, (Member, String)]],
  unregisterHooks: Set[RegisteredUser => EitherT[IO, String, Unit]],
)(using l: Logger[IO], discordLogger: DiscordLogger):
  given QueryParamDecoder[UUID] = QueryParamDecoder[String].emap(uuid => Try(UUID.fromString(uuid)).toEither.left.map(error => new ParseFailure(s"Invalid uuid \"$uuid\"", error.getMessage)))
  def authUri(uuid: UUID, scope: String): Uri =
    (chasterClient.config.authUri / "auth")
      .withQueryParam("client_id", chasterClient.config.clientId)
      .withQueryParam("redirect_uri", registerUri)
      .withQueryParam("response_type", "code")
      .withQueryParam("scope", scope)
      .withQueryParam("state", uuid)

  private object CodeParamMatcher extends QueryParamDecoderMatcher[String]("code")
  private object UUIDParamMatcher extends QueryParamDecoderMatcher[UUID]("state")

  val registerUri: Uri =
    val port = if chasterClient.config.publicPort != 80 then s":${chasterClient.config.publicPort}" else ""
    Uri.unsafeFromString(s"http://${chasterClient.config.publicHost}$port/register")

  private def joinScopes(scope: String, other: String): String = (scope.split(" ") ++ other.split(" ")).distinct.mkString(" ")
  private def containsAllScopes(scope: String, other: String): Boolean =
    scope.split(" ").toSet.subsetOf(other.split(" ").toSet)

  private def addOrUpdateTokenScope(
    accessToken: String,
    expiresAt: Instant,
    refreshToken: String,
    scope: String,
  ): IO[UserToken] =
    userTokenRepository.add(accessToken, expiresAt, refreshToken, scope).attempt.flatMap {
      _.fold(
        throwable => userTokenRepository.get(accessToken.equalAccessToken).flatMap { userToken =>
          userTokenRepository.update(userToken.id, accessToken, expiresAt, refreshToken, joinScopes(userToken.scope, scope))
        },
        _.pure
      )
    }

  def addOrUpdateUser(
    chasterID: ChasterID,
    discordID: DiscordID,
    guildID: DiscordID,
    keyholderIDs: List[ChasterID],
    isLocked: Boolean,
    tokenID: UUID,
  ): IO[RegisteredUser] =
    registeredUserRepository.add(chasterID, discordID, guildID, keyholderIDs, isLocked, tokenID).attempt.flatMap {
      _.fold(
        throwable => registeredUserRepository.find(chasterID.equalChasterID, discordID.equalDiscordID, guildID.equalGuildID).foldF(IO.raiseError(throwable)) { user =>
          registeredUserRepository.update(user.id, tokenID = tokenID.some)
        },
        _.pure
      )
    }

  def requestAccessToken(member: Member, authorizationCode: String, uuid: UUID): IO[Unit] = for
    accessToken <- chasterClient.token(
      "grant_type" -> "authorization_code",
      "code" -> authorizationCode,
      "redirect_uri" -> registerUri.renderString,
    )
    userToken <- addOrUpdateTokenScope(accessToken.access_token, accessToken.expiresAt, accessToken.refresh_token, accessToken.scope)
    profile <- chasterClient(userToken).profile
    locks <- chasterClient(userToken).locks
    keyholderIDs = locks.flatMap(_.keyholder.map(_._id))
    isLocked = locks.exists(_.status == LockStatus.Locked)
    //scopes = accessToken.scope.split(" ")
    registeredUser <- addOrUpdateUser(profile._id, member.discordID, member.guild.discordID, keyholderIDs, isLocked, userToken.id)
    _ <- registrations.update(_ - uuid)
    _ <- discordLogger.logToChannel(s"Registration successful for ${member.mention} -> ${profile.username}")
    _ <- Logger[IO].info(s"Registration successful for $member -> ${profile.username}, UUID: $uuid")
  yield ()

  def routes: HttpRoutes[IO] = HttpRoutes.of {
    case GET -> Root / "register" :? CodeParamMatcher(authorizationCode) +& UUIDParamMatcher(uuid) =>
      registrations.get.flatMap {
        case registrations if !registrations.contains(uuid) => ExpectationFailed()
        case registrations                                  =>
          val member = registrations(uuid)._1
          for
            _ <- requestAccessToken(member, authorizationCode, uuid)
            response <- Ok("Registration Successful")
          yield response
      }

    case GET -> Root / "register" / "authenticate" :? UUIDParamMatcher(uuid) =>
      registrations.get.flatMap {
        case registrations if !registrations.contains(uuid) => Ok("Registration link expired")
        case registrations                                  =>
          val scope = registrations(uuid)._2
          IO(Response[IO](status = Status.Found, headers = Headers(Location(authUri(uuid, scope)))))
      }
  }

  def invalidateRegistration(uuid: UUID, timeout: FiniteDuration): IO[Unit] =
    (IO.sleep(timeout) *> registrations.update(_ - uuid)).start.void

  given QueryParamEncoder[UUID] = (uuid: UUID) => QueryParamEncoder[String].encode(uuid.toString)
  
  def generateURI(member: Member, scope: String, timeout: FiniteDuration): IO[Uri] =
    for
      uuid <- IO(UUID.randomUUID())
      _ <- Logger[IO].info(s"Starting registration for $member, UUID: $uuid")
      _ <- registrations.update(_ + (uuid -> (member, scope)))
      _ <- invalidateRegistration(uuid, timeout)
      authenticateUri = (registerUri / "authenticate").withQueryParam("state", uuid)
    yield authenticateUri

  def register(member: Member, timeout: FiniteDuration): IO[Option[Uri]] =
    val scope = "profile keyholder shared_locks locks"
    registeredUserRepository.find(member.discordID.equalDiscordID)
      .filter(user => containsAllScopes(scope, user.token.scope))
      .value
      .flatMap {
        case Some(_) => IO.pure(None) //Use already registered with all scopes
        case None => generateURI(member, scope, timeout).map(_.some)
      }

  def unregister(member: Member): IO[Option[String]] =
    import cats.syntax.list.*
    val either = for
      registeredUser <- registeredUserRepository.find(member.discordID.equalDiscordID).toRight(s"Unable to find user $member")
      _ <- unregisterHooks.foldLeft(EitherT.liftF[IO, String, Unit](IO.unit))((acc, hook) => acc.flatMap(_ => hook(registeredUser)))
      _ <- EitherT.liftF(userTokenRepository.remove(registeredUser.token.id.equalID))
      _ <- EitherT.liftF(registeredUserRepository.remove(registeredUser.id.equalID))
    yield "If you want you can unregister this application from chaster, you can do that here: https://chaster.app/settings/password"
    either.foldF(
      error => {
        for
          _ <- discordLogger.logToChannel(s"Unable to unregister ${member.mention} $error")
          _ <- Logger[IO].info(s"Unable to unregister $member $error")
        yield None
      },
      _.some.pure
    )

object Registration:
  def apply(
    registeredUserRepository: RegisteredUserRepository,
    userTokenRepository: UserTokenRepository,
    chasterClient: ChasterClient,
    unregisterHooks: Set[RegisteredUser => EitherT[IO, String, Unit]]
  )(using DiscordLogger): IO[Registration] =
    for
      registrations <- Ref.of[IO, Map[UUID, (Member, String)]](Map.empty)
      given Logger[IO]  <- Slf4jLogger.create[IO]
    yield new Registration(
      registeredUserRepository,
      userTokenRepository,
      chasterClient,
      registrations,
      unregisterHooks,
    )
    
