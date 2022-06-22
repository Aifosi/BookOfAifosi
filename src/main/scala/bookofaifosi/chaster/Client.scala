package bookofaifosi.chaster

import bookofaifosi.{Bot, Registration}
import bookofaifosi.db.{RegisteredUserRepository, UserRepository, User as DBUser}
import bookofaifosi.db.Filters.*
import bookofaifosi.chaster.*
import bookofaifosi.model.RegisteredUser
import cats.effect.{IO, Resource}
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
import bookofaifosi.syntax.logger.*
import scala.concurrent.duration.*
import java.time.Instant
import java.util.UUID
import scala.deriving.Mirror

case class WithLastID(
  lastId: Option[String] = None,
  limit: Option[Int] = 50.some,
) derives Encoder.AsObject

case class Result[T: Decoder] (
  hasMore: Boolean,
  results: List[T]
)

object Result:
  inline given decoder[A: Decoder]: Decoder[Result[A]] = (c: HCursor) =>
    for
      more <- c.downField("hasMore").as[Boolean]
      results <- c.downField("results").as[List[A]]
    yield Result(more, results)

case class Paged(
  page: Int,
  limit: Option[Int] = 50.some,
) derives Encoder.AsObject

case class PagesResult[T: Decoder] (
  pages: Int,
  locks: List[T]
)

object PagesResult:
  inline given decoder[A: Decoder]: Decoder[PagesResult[A]] = (c: HCursor) =>
    for
      more <- c.downField("pages").as[Int]
      results <- c.downField("locks").as[List[A]]
    yield PagesResult(more, results)

object Client:
  private val API = Uri.unsafeFromString("https://api.chaster.app")
  private val auth = Uri.unsafeFromString("https://sso.chaster.app/auth/realms/app/protocol/openid-connect")

  def expect[A](req: Request[IO])(using EntityDecoder[IO, A]): IO[A] = Bot.client.get.flatMap(_.expect[A](req))

  def expectAuthenticated[A](user: RegisteredUser, req: Request[IO])(using EntityDecoder[IO, A]): IO[A] =
    for
      user <- user.updatedAccessToken
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
      fullFormData = UrlForm(data ++ formData *)
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
          _ <- Bot.logger.debug(s"Refreshing access token for ${user.discordID}")
          accessToken <- Client.token(
            "grant_type" -> "refresh_token",
            "refresh_token" -> user.refreshToken,
          )
          user <- RegisteredUserRepository.update(user.id, accessToken.access_token, accessToken.expiresAt, accessToken.refresh_token, accessToken.scope)
        yield user

    private def expectUserAuthenticated[A](req: Request[IO])(using EntityDecoder[IO, A]): IO[A] = Client.expectAuthenticated(user, req)

    private def getAll[A <: WithID: Decoder](uri: Uri): Stream[IO, A] =
      for
        user <- Stream.eval(user.updatedAccessToken)
        client <- Stream.eval(Bot.client.get)
        authorization = Authorization(Credentials.Token(AuthScheme.Bearer, user.accessToken))
        responses <- Stream.unfoldEval(client.expect[Result[A]](POST(WithLastID(), uri, authorization)).map(_ -> true)) { result =>
          result.map {
            case (result, false) => None
            case (result, true) =>
              val continue = result.hasMore
              val lastID = result.results.last._id
              val next = client.expect[Result[A]](POST(WithLastID(lastID.some), uri, authorization))
              (result.results, next.map(_ -> continue)).some
          }
        }.metered(60.seconds / 200)
        response <- Stream.emits(responses)
      yield response

    private def getAllBody[A <: WithID: Decoder, Body: Encoder](uri: Uri, body: Body): Stream[IO, A] =
      def createBody(lastId: Option[String] = None, limit: Option[Int] = 50.some): Json =
        body.asJson.deepMerge(WithLastID(lastId, limit).asJson)
      for
        user <- Stream.eval(user.updatedAccessToken)
        client <- Stream.eval(Bot.client.get)
        authorization = Authorization(Credentials.Token(AuthScheme.Bearer, user.accessToken))
        responses <- Stream.unfoldEval(client.expect[Result[A]](POST(createBody(), uri, authorization)).map(_ -> true)) { result =>
          result.map {
            case (result, false) => None
            case (result, true) =>
              val continue = result.hasMore
              val lastID = result.results.last._id
              val next = client.expect[Result[A]](POST(createBody(lastID.some), uri, authorization))
              (result.results, next.map(_ -> continue)).some
          }
        }.metered(60.seconds / 200)
        response <- Stream.emits(responses)
      yield response

    //def profile: IO[User] = expectUserAuthenticated[User](GET(API / "auth" / "profile"))
    def locks: IO[List[Lock]] = expectUserAuthenticated[List[Lock]](GET(API / "locks"))
    def lock(id: String): IO[Lock] = expectUserAuthenticated[Lock](GET(API / "locks" / id))
    def lockHistory(id: String, eventsAfter: Option[Instant] = None): Stream[IO, Event[Json]] =
      getAll[Event[Json]](API / "locks" / id / "history")
        .takeWhile(event => eventsAfter.forall(_.isBefore(event.createdAt)))
    def keyholderLocks: Stream[IO, Lock] = //getAll[Lock](API / "keyholder" / "locks" / "search")
      for
        user <- Stream.eval(user.updatedAccessToken)
        client <- Stream.eval(Bot.client.get)
        uri = API / "keyholder" / "locks" / "search"
        authorization = Authorization(Credentials.Token(AuthScheme.Bearer, user.accessToken))
        responses <- Stream.unfoldEval(client.expect[PagesResult[Lock]](POST(Paged(0), uri, authorization)).map(_ -> 0)) { result =>
          result.map {
            case (result, page) if page == result.pages + 1 => None
            case (result, page) =>
              val newPage = page + 1
              val next = client.expect[PagesResult[Lock]](POST(Paged(newPage), uri, authorization))
              (result.locks, next.map(_ -> newPage)).some
          }
        }.metered(60.seconds / 200)
        response <- Stream.emits(responses)
      yield response