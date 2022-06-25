package bookofaifosi

import bookofaifosi.Bot
import bookofaifosi.chaster.{AccessToken, Client, User as ChasterUser}
import bookofaifosi.chaster.Client.given
import bookofaifosi.db.{RegisteredUserRepository, UserRoleRepository}
import bookofaifosi.db.Filters.*
import bookofaifosi.db.given_Put_DiscordID
import bookofaifosi.model.{DiscordID, Guild, Member, RegisteredUser, User, UserRole}
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
import doobie.syntax.string.*
import doobie.{Get, Put}
import java.time.Instant
import scala.concurrent.duration.*
import java.util.UUID
import scala.util.Try
import bookofaifosi.syntax.logger.*
import bookofaifosi.db.Filter

object Registration:
  enum Role(val scope: String):
    case Wearer extends Role("profile locks")
    case Keyholder extends Role("profile keyholder shared_locks")
    def equalUserType: Filter = fr"user_type = $this".some

  object Role:
    given Put[Role] = Put[String].contramap(_.toString.toLowerCase)


  private val registrations: Ref[IO, Map[UUID, (Member, String, Role)]] = Ref.unsafe(Map.empty)

  given QueryParamDecoder[UUID] = QueryParamDecoder[String].emap(uuid => Try(UUID.fromString(uuid)).toEither.left.map(error => new ParseFailure(s"Invalid uuid \"$uuid\"", error.getMessage)))
  private object CodeParamMatcher extends QueryParamDecoderMatcher[String]("code")
  private object UUIDParamMatcher extends QueryParamDecoderMatcher[UUID]("state")

  val registerUri =
    val port = if Bot.config.publicPort != 80 then s":${Bot.config.publicPort}" else ""
    Uri.unsafeFromString(s"http://${Bot.config.publicHost}$port/register")

  private def joinScopes(scope: String, other: String): String = (scope.split(" ") ++ other.split(" ")).distinct.mkString(" ")

  def addOrUpdateScope(
    chasterName: String,
    discordID: DiscordID,
    accessToken: String,
    expiresAt: Instant,
    refreshToken: String,
    scope: String,
  ): IO[RegisteredUser] =
    RegisteredUserRepository.add(chasterName, discordID, accessToken, expiresAt, refreshToken, scope).attempt.flatMap {
      _.fold(
        throwable => RegisteredUserRepository.find(chasterName.equalChasterName, discordID.equalUserID).flatMap(_.fold(IO.raiseError(throwable)) { user =>
          RegisteredUserRepository.update(user.id, accessToken, expiresAt, refreshToken, joinScopes(user.scope, scope))
        }),
        _.pure
      )
    }

  val routes: HttpRoutes[IO] = HttpRoutes.of {
    case GET -> Root / "register" :? CodeParamMatcher(authorizationCode) +& UUIDParamMatcher(uuid) =>
      def requestAccessToken(member: Member, role: Role): IO[Unit] = for
        httpClient <- Bot.client.get
        accessToken <- Client.token(
          "grant_type" -> "authorization_code",
          "code" -> authorizationCode,
          "redirect_uri" -> registerUri.renderString,
        )
        profileUri = Uri.unsafeFromString("https://api.chaster.app/auth/profile")
        profile <- httpClient.expect[ChasterUser](GET(profileUri, Authorization(Credentials.Token(AuthScheme.Bearer, accessToken.access_token))))
        registeredUser <- addOrUpdateScope(profile.username, member.discordID, accessToken.access_token, accessToken.expiresAt, accessToken.refresh_token, accessToken.scope)
        _ <- registrations.update(_ - uuid)
        guildRole <- UserRoleRepository.find(member.guild.discordID.equalGuildID, role.equalUserType)
        _ <- guildRole.fold(IO.unit)(role => member.addRole(role.role))
        _ <- Bot.logger.info(s"Registration successful for $member -> ${profile.username}, UUID: $uuid")
      yield ()

      registrations.get.flatMap {
        case registrations if !registrations.contains(uuid) => ExpectationFailed()
        case registrations                                  =>
          val (member, _, role) = registrations(uuid)
          for
            registeredUser <- requestAccessToken(member, role).start
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

  def register(member: Member, timeout: FiniteDuration, role: Role): IO[Uri] =
    for
      uuid <- IO(UUID.randomUUID())
      _ <- Bot.logger.info(s"Starting registration for $member, UUID: $uuid, role: $role")
      registeredUser <- RegisteredUserRepository.find(member.discordID.equalUserID)
      fullScope = registeredUser.fold(role.scope)(user => joinScopes(user.scope, role.scope))
      _ <- registrations.update(_ + (uuid -> (member, fullScope, role)))
      _ <- invalidateRegistration(uuid, timeout)
      authenticateUri = (registerUri / "authenticate").withQueryParam("state", uuid)
    yield authenticateUri
