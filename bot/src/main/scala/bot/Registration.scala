package bot

import bot.Bot
import bot.chaster.{AccessToken, Client, Lock, LockStatus, User as ChasterUser}
import bot.chaster.Client.*
import bot.chaster.Client.given
import bot.db.{RegisteredUserRepository, User, UserTokenRepository}
import bot.db.Filters.*
import bot.db.given_Put_DiscordID
import bot.model.{ChasterID, DiscordID, Guild, Member, RegisteredUser, User, UserToken}
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
import bot.db.Filter
import cats.data.{EitherT, OptionT}
import org.typelevel.log4cats.Logger

object Registration:
  private val registrations: Ref[IO, Map[UUID, (Member, String)]] = Ref.unsafe(Map.empty)

  given QueryParamDecoder[UUID] = QueryParamDecoder[String].emap(uuid => Try(UUID.fromString(uuid)).toEither.left.map(error => new ParseFailure(s"Invalid uuid \"$uuid\"", error.getMessage)))
  private object CodeParamMatcher extends QueryParamDecoderMatcher[String]("code")
  private object UUIDParamMatcher extends QueryParamDecoderMatcher[UUID]("state")

  val registerUri: Uri =
    val port = if Bot.chaster.publicPort != 80 then s":${Bot.chaster.publicPort}" else ""
    Uri.unsafeFromString(s"http://${Bot.chaster.publicHost.toString}$port/register")

  private def joinScopes(scope: String, other: String): String = (scope.split(" ") ++ other.split(" ")).distinct.mkString(" ")
  private def containsAllScopes(scope: String, other: String): Boolean =
    scope.split(" ").toSet.subsetOf(other.split(" ").toSet)

  private def addOrUpdateTokenScope(
    accessToken: String,
    expiresAt: Instant,
    refreshToken: String,
    scope: String,
  ): IO[UserToken] =
    UserTokenRepository.add(accessToken, expiresAt, refreshToken, scope).attempt.flatMap {
      _.fold(
        throwable => UserTokenRepository.get(accessToken.equalAccessToken).flatMap { userToken =>
          UserTokenRepository.update(userToken.id, accessToken, expiresAt, refreshToken, joinScopes(userToken.scope, scope))
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
    RegisteredUserRepository.add(chasterID, discordID, guildID, keyholderIDs, isLocked, tokenID).attempt.flatMap {
      _.fold(
        throwable => RegisteredUserRepository.find(chasterID.equalChasterID, discordID.equalDiscordID, guildID.equalGuildID).flatMap(_.fold(IO.raiseError(throwable)) { user =>
          RegisteredUserRepository.update(user.id, tokenID = tokenID.some)
        }),
        _.pure
      )
    }

  def requestAccessToken(member: Member, authorizationCode: String, uuid: UUID)(using Logger[IO]): IO[Unit] = for
    httpClient <- Bot.client.get
    accessToken <- Client.token(
      "grant_type" -> "authorization_code",
      "code" -> authorizationCode,
      "redirect_uri" -> registerUri.renderString,
    )
    userToken <- addOrUpdateTokenScope(accessToken.access_token, accessToken.expiresAt, accessToken.refresh_token, accessToken.scope)
    profile <- userToken.profile
    locks <- userToken.locks
    keyholderIDs = locks.flatMap(_.keyholder.map(_._id))
    isLocked = locks.exists(_.status == LockStatus.Locked)
    scopes = accessToken.scope.split(" ")
    registeredUser <- addOrUpdateUser(profile._id, member.discordID, member.guild.discordID, keyholderIDs, isLocked, userToken.id)
    _ <- registrations.update(_ - uuid)
    _ <- Bot.channels.log.sendMessage(s"Registration successful for ${member.mention} -> ${profile.username}").value
    _ <- Logger[IO].info(s"Registration successful for $member -> ${profile.username}, UUID: $uuid")
  yield ()

  def routes(using Logger[IO]): HttpRoutes[IO] = HttpRoutes.of {
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
          IO(Response[IO](status = Status.Found, headers = Headers(Location(Client.authUri(uuid, scope)))))
      }
  }

  def invalidateRegistration(uuid: UUID, timeout: FiniteDuration): IO[Unit] =
    (IO.sleep(timeout) *> registrations.update(_ - uuid)).start.void

  def register(member: Member, timeout: FiniteDuration)(using Logger[IO]): IO[Option[Uri]] =
    for
      registeredUser <- RegisteredUserRepository.find(member.discordID.equalDiscordID)
      scope = "profile keyholder shared_locks locks"
      authenticateUri <- if registeredUser.exists(user => containsAllScopes(scope, user.scope)) then
        None.pure[IO]
      else
        for
          uuid <- IO(UUID.randomUUID())
          _ <- Logger[IO].info(s"Starting registration for $member, UUID: $uuid")
          _ <- registrations.update(_ + (uuid -> (member, scope)))
          _ <- invalidateRegistration(uuid, timeout)
          authenticateUri = (registerUri / "authenticate").withQueryParam("state", uuid)
        yield authenticateUri.some
    yield authenticateUri

  private var unregisterHooks: Set[RegisteredUser => EitherT[IO, String, Unit]] = Set.empty

  def addUnregisterHook(hook: RegisteredUser => EitherT[IO, String, Unit]): Unit =
    unregisterHooks += hook

  def unregister(member: Member)(using Logger[IO]): IO[Option[String]] =
    import cats.syntax.list.*
    val either = for
      registeredUser <- OptionT(RegisteredUserRepository.find(member.discordID.equalDiscordID)).toRight(s"Unable to find user $member")
      _ <- unregisterHooks.foldLeft(EitherT.liftF[IO, String, Unit](IO.unit))((acc, hook) => acc.flatMap(_ => hook(registeredUser)))
      _ <- EitherT.liftF(UserTokenRepository.remove(registeredUser.token.id.equalID))
      _ <- EitherT.liftF(RegisteredUserRepository.remove(registeredUser.id.equalID))
      //_ <- EitherT.liftF((registeredUser.sendMessage("If you want you can unregister this application from chaster, you can do that here: https://chaster.app/settings/password")))
    yield "If you want you can unregister this application from chaster, you can do that here: https://chaster.app/settings/password"
    either.foldF(
      error => {
        for
          _ <- Bot.channels.log.sendMessage(s"Unable to unregister ${member.mention} $error").value
          _ <- Logger[IO].info(s"Unable to unregister $member $error")
        yield None
      },
      _.some.pure
    )
    
