package bookofaifosi

import bookofaifosi.Bot
import bookofaifosi.chaster.{Client, Profile}
import bookofaifosi.db.User as DBUser
import bookofaifosi.wrappers.User
import cats.effect.{IO, Ref}
import doobie.syntax.connectionio.*
import io.circe.Decoder
import org.http4s.*
import org.http4s.dsl.io.*
import org.http4s.circe.CirceEntityDecoder.*
import org.http4s.Method.*
import org.http4s.client.dsl.io.*
import org.http4s.headers.Location

import scala.concurrent.duration.*
import java.time.OffsetDateTime
import java.util.UUID
import scala.util.Try

object Registration:
  private val registrations: Ref[IO, Map[UUID, (User, String)]] = Ref.unsafe(Map.empty)

  given QueryParamDecoder[UUID] = QueryParamDecoder[String].emap(uuid => Try(UUID.fromString(uuid)).toEither.left.map(error => new ParseFailure(s"Invalid uuid \"$uuid\"", error.getMessage)))
  given QueryParamEncoder[UUID] = (uuid: UUID) => QueryParamEncoder[String].encode(uuid.toString)
  private object CodeParamMatcher extends QueryParamDecoderMatcher[String]("code")
  private object UUIDParamMatcher extends QueryParamDecoderMatcher[UUID]("state")

  case class AccessToken(
    access_token: String,
    expires_in: Int,
    refresh_expires_in: Int,
    refresh_token: String,
    token_type: String,
    scope: String,
  ) derives Decoder

  private val baseUri = Uri.unsafeFromString("https://sso.chaster.app/auth/realms/app/protocol/openid-connect")
  private val registerUri = Uri.unsafeFromString(s"http://${Bot.config.publicHost}/register")

  val routes: HttpRoutes[IO] = HttpRoutes.of {
    case GET -> Root / "register" :? CodeParamMatcher(authorizationCode) +& UUIDParamMatcher(uuid) =>
      def requestAccessToken(user: User): IO[Unit] = for
        httpClient <- Bot.client.get
        uri = baseUri / "token"
        formData = UrlForm(
          "client_id" -> Bot.chasterConfig.clientId,
          "client_secret" -> Bot.chasterConfig.secretKey,
          "grant_type" -> "authorization_code",
          "code" -> authorizationCode,
          "redirect_uri" -> registerUri.renderString
        )
        accessToken <- httpClient.expect[AccessToken](POST.apply(uri).withEntity(formData))
        expiresAt = OffsetDateTime.now().plusMinutes(accessToken.expires_in)
        client = Client(httpClient, accessToken.access_token, expiresAt, accessToken.refresh_token)
        profile <- client.profile
        _ <- DBUser.add(profile.username, user.id, accessToken.access_token, expiresAt, accessToken.refresh_token, accessToken.scope).transact(Bot.xa)
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
        case registrations if !registrations.contains(uuid) => NotFound()
        case registrations                                  =>
          val scope = registrations(uuid)._2
          val uri = (baseUri / "auth")
            .withQueryParam("client_id", Bot.chasterConfig.clientId)
            .withQueryParam("redirect_uri", registerUri)
            .withQueryParam("response_type", "code")
            .withQueryParam("scope", scope)
            .withQueryParam("state", uuid)
          IO(Response[IO](status = Status.Found, headers = Headers(Location(uri))))
      }
  }

  def invalidateRegistration(uuid: UUID, timeout: FiniteDuration): IO[Unit] =
    (IO.sleep(timeout) *> registrations.update(_ - uuid)).start.void

  def basic(user: User, timeout: FiniteDuration): IO[Uri] =
    for
      uuid <- IO(UUID.randomUUID())
      scope = "profile"
      _ <- IO.println(s"Starting registration for $user, UUID: $uuid, scope: $scope")
      _ <- registrations.update(_ + (uuid -> (user, scope)))
      _ <- invalidateRegistration(uuid, timeout)
      authenticateUri = (registerUri / "authenticate").withQueryParam("state", uuid)
    yield authenticateUri
