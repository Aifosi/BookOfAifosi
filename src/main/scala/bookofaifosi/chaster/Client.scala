package bookofaifosi.chaster

import bookofaifosi.{Bot, Registration}
import bookofaifosi.db.{RegisteredUserRepository, UserRepository, User as DBUser}
import bookofaifosi.db.Filters.*
import bookofaifosi.chaster.Paged.getAll as getAllPaged
import bookofaifosi.chaster.*
import bookofaifosi.model.RegisteredUser
import cats.effect.IO
import cats.syntax.option.*
import io.circe.syntax.*
import org.http4s.*
import org.http4s.dsl.io.*
import org.http4s.circe.CirceEntityDecoder.*
import org.http4s.circe.CirceEntityEncoder.*
import org.http4s.Method.*
import org.http4s.client.dsl.io.*
import org.http4s.client
import org.http4s.headers.Authorization
import doobie.syntax.connectionio.*
import fs2.Stream
import io.circe.{Decoder, Encoder, HCursor, Json}
import org.http4s.client.Client

import scala.concurrent.duration.*
import java.time.Instant
import java.util.UUID
import scala.deriving.Mirror

object Client:
  private val API = Uri.unsafeFromString("https://api.chaster.app")
  private val auth = Uri.unsafeFromString("https://sso.chaster.app/auth/realms/app/protocol/openid-connect")

  def expect[A](req: Request[IO])(using EntityDecoder[IO, A]): IO[A] = Bot.client.get.flatMap(_.expect[A](req))

  def expectAuthenticated[A](user: RegisteredUser, req: Request[IO])(using EntityDecoder[IO, A]): IO[A] =
    for
      user <- user.updatedAccessToken
      client <- Bot.client.get
      request = req.putHeaders(Authorization(Credentials.Token(AuthScheme.Bearer, user.accessToken)))
      response <- expect[A](request)
    yield response

  def token(formData: (String, String)*): IO[AccessToken] =
    for
      client <- Bot.client.get
      uri = auth / "token"
      data = Seq(
        "client_id" -> Bot.chasterConfig.clientId,
        "client_secret" -> Bot.chasterConfig.secretKey,
      )
      fullFormData = UrlForm((data ++ formData)*)
      accessToken <- client.expect[AccessToken](POST.apply(uri).withEntity(fullFormData))
    yield accessToken

  given QueryParamEncoder[UUID] = (uuid: UUID) => QueryParamEncoder[String].encode(uuid.toString)
  def authUri(uuid: UUID, scope: String): Uri =
    (auth / "auth")
      .withQueryParam("client_id", Bot.chasterConfig.clientId)
      .withQueryParam("redirect_uri", Registration.registerUri)
      .withQueryParam("response_type", "code")
      .withQueryParam("scope", scope)
      .withQueryParam("state", uuid)

  extension (user: RegisteredUser)
    private def updatedAccessToken: IO[RegisteredUser] =
      if user.expiresAt.isAfter(Instant.now()) then
        IO.pure(user)
      else
        for
          _ <- IO.println(s"Refreshing access token for ${user.discordID}")
          accessToken <- Client.token(
            "grant_type" -> "refresh_token",
            "refresh_token" -> user.refreshToken,
          )
          user <- RegisteredUserRepository.update(user.id, accessToken.access_token, accessToken.expiresAt, accessToken.refresh_token, accessToken.scope)
        yield user

    private def expectUserAuthenticated[A](req: Request[IO])(using EntityDecoder[IO, A]): IO[A] = Client.expectAuthenticated(user, req)
    private def getAll[A <: WithID: Decoder](uri: Uri)(lastIDSeen: Option[String]): Stream[IO, A] =
      for
        user <- Stream.eval(user.updatedAccessToken)
        client <- Stream.eval(Bot.client.get)
        response <- client.getAllPaged[A](uri, Authorization(Credentials.Token(AuthScheme.Bearer, user.accessToken)))(lastIDSeen)
      yield response

    //def profile: IO[User] = expectUserAuthenticated[User](GET(API / "auth" / "profile"))
    def locks: IO[List[Lock]] = expectUserAuthenticated[List[Lock]](GET(API / "locks"))
    def lock(id: String): IO[Lock] = expectUserAuthenticated[Lock](GET(API / "locks" / id))
    def lockHistory(id: String, eventsAfter: Option[Instant] = None): Stream[IO, Event[Json]] =
      getAll[Event[Json]](API / "locks" / id / "history")(None)
        .takeWhile(event => eventsAfter.forall(_.isBefore(event.createdAt)))