package bookofaifosi

import bookofaifosi.Bot
import bookofaifosi.chaster.{AccessToken, Client, User as ChasterUser}
import bookofaifosi.chaster.Client.given
import bookofaifosi.db.{RegisteredUserRepository, UserRepository}
import bookofaifosi.db.Filters.*
import bookofaifosi.model.{RegisteredUser, User}
import cats.effect.{IO, Ref}
import doobie.syntax.connectionio.*
import io.circe.Decoder
import org.http4s.*
import org.http4s.dsl.io.*
import org.http4s.circe.CirceEntityDecoder.*
import org.http4s.Method.*
import org.http4s.client.dsl.io.*
import org.http4s.headers.{Authorization, Location}
import cats.syntax.applicative.*
import java.time.Instant
import scala.concurrent.duration.*
import java.util.UUID
import scala.util.Try

object Registration:
  private val registrations: Ref[IO, Map[UUID, (User, String)]] = Ref.unsafe(Map.empty)

  given QueryParamDecoder[UUID] = QueryParamDecoder[String].emap(uuid => Try(UUID.fromString(uuid)).toEither.left.map(error => new ParseFailure(s"Invalid uuid \"$uuid\"", error.getMessage)))
  private object CodeParamMatcher extends QueryParamDecoderMatcher[String]("code")
  private object UUIDParamMatcher extends QueryParamDecoderMatcher[UUID]("state")

  val registerUri = Uri.unsafeFromString(s"http://${Bot.config.publicHost}/register")

  private def joinScopes(scope: String, other: String): String = (scope.split(" ") ++ other.split(" ")).distinct.mkString(" ")

  def addOrUpdateScope(
    chasterName: String,
    discordID: Long,
    accessToken: String,
    expiresAt: Instant,
    refreshToken: String,
    scope: String,
  ): IO[RegisteredUser] =
    RegisteredUserRepository.add(chasterName, discordID, accessToken, expiresAt, refreshToken, scope).attempt.flatMap {
      _.fold(
        throwable => RegisteredUserRepository.find(chasterName.equalChasterName, discordID.equalID).flatMap(_.fold(IO.raiseError(throwable)) { user =>
          RegisteredUserRepository.update(user.id, accessToken, expiresAt, refreshToken, joinScopes(user.scope, scope))
        }),
        _.pure
      )
    }

  val routes: HttpRoutes[IO] = HttpRoutes.of {
    case GET -> Root / "register" :? CodeParamMatcher(authorizationCode) +& UUIDParamMatcher(uuid) =>
      def requestAccessToken(user: User): IO[Unit] = for
        httpClient <- Bot.client.get
        accessToken <- Client.token(
          "grant_type" -> "authorization_code",
          "code" -> authorizationCode,
          "redirect_uri" -> registerUri.renderString,
        )
        profileUri = Uri.unsafeFromString("https://api.chaster.app/auth/profile")
        profile <- httpClient.expect[ChasterUser](GET(profileUri, Authorization(Credentials.Token(AuthScheme.Bearer, accessToken.access_token))))
        _ <- addOrUpdateScope(profile.username, user.discordID, accessToken.access_token, accessToken.expiresAt, accessToken.refresh_token, accessToken.scope)
        _ <- registrations.update(_ - uuid)
        _ <- IO.println(s"Registration successful for $user -> ${profile.username}, UUID: $uuid")
      yield ()

      registrations.get.flatMap {
        case registrations if !registrations.contains(uuid) => ExpectationFailed()
        case registrations                                  =>
          requestAccessToken(registrations(uuid)._1).start *> Ok("Registration Successful")
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

  def register(user: User, timeout: FiniteDuration, scope: String): IO[Uri] =
    for
      uuid <- IO(UUID.randomUUID())
      _ <- IO.println(s"Starting registration for $user, UUID: $uuid, scope: $scope")
      registeredUser <- RegisteredUserRepository.find(user.discordID.equalID)
      fullScope = registeredUser.fold(scope)(user => joinScopes(user.scope, scope))
      _ <- registrations.update(_ + (uuid -> (user, fullScope)))
      _ <- invalidateRegistration(uuid, timeout)
      authenticateUri = (registerUri / "authenticate").withQueryParam("state", uuid)
    yield authenticateUri
