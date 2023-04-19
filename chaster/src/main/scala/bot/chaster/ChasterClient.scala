package bot.chaster

import bot.chaster.*
import bot.model.{ChasterID, UserToken}
import cats.data.EitherT
import cats.effect.{IO, Resource}
import cats.syntax.applicative.*
import cats.syntax.option.*
import cats.syntax.either.*
import cats.syntax.bifunctor.*
import fs2.Stream
import io.circe.syntax.*
import io.circe.{Decoder, Encoder, HCursor, Json}
import org.http4s.*
import org.http4s.Method.*
import org.http4s.Status.*
import org.http4s.circe.CirceEntityDecoder.*
import org.http4s.circe.CirceEntityEncoder.*
import org.http4s.client.dsl.io.*
import org.http4s.client.{Client, UnexpectedStatus}
import org.http4s.dsl.io.{GET, *}
import org.http4s.ember.client.EmberClientBuilder
import org.http4s.headers.Authorization
import org.typelevel.ci.*
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger

import java.time.{Instant, LocalDateTime}
import java.time.format.DateTimeFormatter
import java.time.temporal.ChronoUnit
import java.util.UUID
import scala.concurrent.duration.*
import scala.deriving.Mirror
import scala.reflect.Typeable

case class WithLastID(
  lastId: Option[ChasterID] = None,
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

class ChasterClient private(
  client: Client[IO],
  val config: ChasterConfiguration,
  onTokenRefresh: (UUID, AccessToken) => IO[UserToken],
):
  chasterClient =>

  private val API = config.apiUri

  extension (client: Client[IO])
    def expectOrChasterError[E, A](req: Request[IO])(recover: PartialFunction[ChasterAPIError, EitherT[IO, E, A]])(using EntityDecoder[IO, A]): EitherT[IO, E, A] =
      EitherT {
        client.expectOr[A](req) {
          case ClientError(response) =>
            EntityDecoder[IO, ChasterAPIError].decode(response, strict = false).leftWiden[Throwable].rethrowT
          case resp => IO.pure(UnexpectedStatus(resp.status, req.method, req.uri))
        }.map(_.asRight).recoverWith {
          case chasterError: ChasterAPIError => recover.applyOrElse(chasterError, _ => EitherT.liftF(IO.raiseError(chasterError))).value
        }
      }

  def token(formData: (String, String)*): IO[AccessToken] =
    val uri = config.authUri / "token"
    val data = Seq(
      "client_id" -> config.clientId,
      "client_secret" -> config.secretKey,
    )
    val fullFormData = UrlForm(data ++ formData*)
    client.expect[AccessToken](POST.apply(uri).withEntity(fullFormData))

  given EntityDecoder[IO, Unit] = EntityDecoder.void

  def publicProfileByID(id: ChasterID): IO[PublicUser] = client.expect[PublicUser](GET(API / "users" / "profile" / "by-id" / id))
  def publicProfileByName(name: String): IO[Option[PublicUser]] = client.expectOrChasterError[String, PublicUser](GET(API / "users" / "profile" / name)) {
    case ChasterAPIError(Status.NotFound, message, _) => EitherT.leftT(message)
  }.toOption.value

  def apply(token: UserToken): AuthenticatedEndpoints = new AuthenticatedEndpoints(token)
  def authenticatedEndpoints(token: UserToken): AuthenticatedEndpoints = new AuthenticatedEndpoints(token)

  class AuthenticatedEndpoints(token: UserToken):
    authenticatedEndpoints =>
    private def withUpdatedToken: IO[AuthenticatedEndpoints] =
      IO.println(token.expiresAt) *> IO.println(Instant.now()) *> IO.println(token.expiresAt.isAfter(Instant.now())) *> (if token.expiresAt.isAfter(Instant.now()) then
        IO.pure(this)
      else
        for
          logger <- Slf4jLogger.create[IO]
          _ <- IO(Thread.dumpStack()).attempt.flatMap(_ => IO.unit)
          _ <- logger.debug(s"Refreshing access token with ID: ${token.id}")
          accessToken <- chasterClient.token(
            "grant_type" -> "refresh_token",
            "refresh_token" -> token.refreshToken,
          )
          token <- onTokenRefresh(token.id, accessToken)
        yield AuthenticatedEndpoints(token))

    private def authorizationHeader: Authorization = Authorization(Credentials.Token(AuthScheme.Bearer, token.accessToken))

    private def expectAuthenticatedOrChasterError[E, A](
      req: Request[IO]
    )(
      recover: PartialFunction[ChasterAPIError, EitherT[IO, E, A]]
    )(
      using EntityDecoder[IO, A]
    ): EitherT[IO, E, A] =
      for
        token <- EitherT.liftF(withUpdatedToken)
        request = req.putHeaders(token.authorizationHeader)
        response <- client.expectOrChasterError[E, A](request)(recover)
      yield response

    private def expectAuthenticated[A](req: Request[IO])(using EntityDecoder[IO, A]): IO[A] =
      expectAuthenticatedOrChasterError[Throwable, A](req)(PartialFunction.empty).rethrowT
    private def getAll[A <: WithID: Decoder](uri: Uri): Stream[IO, A] =
      for
        authenticatedEndpoints <- Stream.eval(withUpdatedToken)
        authorization = authenticatedEndpoints.authorizationHeader
        responses <- Stream.unfoldEval(client.expect[Result[A]](POST(WithLastID(), uri, authorization)).map(_ -> true)) { result =>
          result.map {
            case (result, false) => None
            case (result, true) =>
              val continue = result.hasMore
              val lastID = result.results.last._id
              val next = client.expect[Result[A]](POST(WithLastID(lastID.some), uri, authorization))
              (result.results, next.map(_ -> continue)).some
          }
        }.spaced(60.seconds / 200)
        response <- Stream.emits(responses)
      yield response
  
    private def getAllBody[A <: WithID: Decoder, Body: Encoder](uri: Uri, body: Body): Stream[IO, A] =
      def createBody(lastId: Option[ChasterID] = None, limit: Option[Int] = 50.some): Json =
        body.asJson.deepMerge(WithLastID(lastId, limit).asJson)
      for
        authenticatedEndpoints <- Stream.eval(withUpdatedToken)
        authorization = authenticatedEndpoints.authorizationHeader
        responses <- Stream.unfoldEval(client.expect[Result[A]](POST(createBody(), uri, authorization)).map(_ -> true)) { result =>
          result.map {
            case (result, false) => None
            case (result, true) =>
              val continue = result.hasMore
              val lastID = result.results.last._id
              val next = client.expect[Result[A]](POST(createBody(lastID.some), uri, authorization))
              (result.results, next.map(_ -> continue)).some
          }
        }.spaced(60.seconds / 200)
        response <- Stream.emits(responses)
      yield response

    def profile: IO[User] = expectAuthenticated[User](GET(API / "auth" / "profile"))
    def locks: IO[List[Lock]] = expectAuthenticated[List[Lock]](GET(API / "locks"))
    def lock(id: ChasterID): IO[Lock] = expectAuthenticated[Lock](GET(API / "locks" / id))
    def lockHistory(id: ChasterID, eventsAfter: Option[Instant] = None): Stream[IO, Event[Json]] =
      getAll[Event[Json]](API / "locks" / id / "history")
        .takeWhile(event => eventsAfter.forall(_.isBefore(event.createdAt)))
    def keyholderLocks: Stream[IO, Lock] =
      for
        authenticatedEndpoints <- Stream.eval(withUpdatedToken)
        uri = API / "keyholder" / "locks" / "search"
        authorization = authenticatedEndpoints.authorizationHeader
        responses <- Stream.unfoldEval(client.expect[PagesResult[Lock]](POST(Paged(0), uri, authorization)).map(_ -> 0)) { result =>
          result.map {
            case (result, page) if page == result.pages + 1 => None
            case (result, page) =>
              val newPage = page + 1
              val next = client.expect[PagesResult[Lock]](POST(Paged(newPage), uri, authorization))
              (result.locks, next.map(_ -> newPage)).some
          }
        }.spaced(60.seconds / 200)
        response <- Stream.emits(responses)
      yield response
    def modifyTime(lock: ChasterID, modification: FiniteDuration): IO[Unit] =
      expectAuthenticated[Unit](POST(Json.obj("duration" -> Json.fromLong(modification.toSeconds)), API / "locks" / lock / "update-time"))
    def posts: Stream[IO, Post] = getAll(API / "posts")
    def post(id: ChasterID): IO[Post] = expectAuthenticated(GET(API / "posts" / id))
    def setFreeze(lock: ChasterID, freeze: Boolean): IO[Unit] = expectAuthenticated(POST(Json.obj("isFrozen" -> Json.fromBoolean(freeze)), API / "locks" / lock / "freeze"))
    def freeze(lock: ChasterID): IO[Unit] = setFreeze(lock, true)
    def unfreeze(lock: ChasterID): IO[Unit] = setFreeze(lock, false)
    def action[E, Response: Decoder](
      lock: ChasterID,
      extension: ChasterID,
    )(
      action: String,
      payload: ExtensionActionPayload,
    )(
      recover: PartialFunction[ChasterAPIError, EitherT[IO, E, Response]]
    ): EitherT[IO, E, Response] =
      expectAuthenticatedOrChasterError(POST(ExtensionAction(action, payload), API / "locks" / lock / "extensions" / extension / "action"))(recover)
    def settings(lockID: ChasterID, settingsUpdate: SettingsUpdate): IO[Unit] =
      expectAuthenticated(POST(settingsUpdate, API / "locks" / lockID / "settings"))
    def updateSettings(lockID: ChasterID, settingsUpdate: SettingsUpdate => SettingsUpdate): IO[Unit] =
      for
        lock <- lock(lockID)
        currentSettings = SettingsUpdate(
          lock.displayRemainingTime,
          lock.hideTimeLogs,
        )
        _ <- settings(lockID, settingsUpdate(currentSettings))
      yield ()
    def extensions(lock: ChasterID, extensionUpdates: ConfigUpdate[ExtensionConfig]*): IO[Unit] =
      expectAuthenticated(POST(ConfigUpdatePayload(extensionUpdates.toList), API / "locks" / lock / "extensions"))
    def updateExtensions(lockID: ChasterID)(update: List[ConfigUpdate[ExtensionConfig]] => List[ConfigUpdate[ExtensionConfig]]): IO[Unit] =
      for
        lock <- lock(lockID)
        configs = lock.extensions.map { extension =>
          ConfigUpdate(extension.config, extension.mode, extension.regularity)
        }
        updatedConfigs = update(configs)
        _ <- extensions(lockID, updatedConfigs*)
      yield ()
    def updateExtension[Config <: ExtensionConfig: Typeable](lockID: ChasterID)(update: ConfigUpdate[Config] => ConfigUpdate[Config]): IO[Unit] =
      for
        lock <- lock(lockID)
        updatedConfigs = lock.extensions.map { extension =>
          extension.config match
            case config: Config => update(ConfigUpdate(config, extension.mode, extension.regularity))
            case config => ConfigUpdate(config, extension.mode, extension.regularity)
        }
        _ <- extensions(lockID, updatedConfigs*)
      yield ()
    def sharedLink(sharedLink: ChasterID): IO[SharedLink] =
      expectAuthenticated(GET(API / "shared-links" / sharedLink))

    def vote(lock: ChasterID, extension: ChasterID, action: VoteAction, sharedLink: ChasterID): EitherT[IO, String, FiniteDuration] =
      authenticatedEndpoints.action[String, VoteResponse](lock, extension)("vote", VotePayload(action, sharedLink)){
        case ChasterAPIError(Status.BadRequest, error @ "Cannot vote now", _) => EitherT.leftT(error)
      }.map(_.duration)

object ChasterClient:
  private def acquireHttpClient: Resource[IO, Client[IO]] =
    def retryPolicy(
      request: Request[IO],
      response: Either[Throwable, Response[IO]],
      retries: Int,
    ): Option[FiniteDuration] =
      response.toOption.flatMap {
        case response if response.status == Status.TooManyRequests =>
          response.headers.get(ci"x-ratelimit-reset")
            .map(_.head.value)
            .map(LocalDateTime.parse(_, DateTimeFormatter.ofPattern("EEE, dd MMM yyy HH:mm:ss zzz")))
            .map(ChronoUnit.SECONDS.between(LocalDateTime.now, _).seconds)
        case _                                                     => None
      }

    EmberClientBuilder.default[IO].withRetryPolicy(retryPolicy).build

  def apply(onTokenRefresh: (UUID, AccessToken) => IO[UserToken]): IO[ChasterClient] =
    acquireHttpClient.use { httpClient =>
      ChasterConfiguration.fromConfig().map(new ChasterClient(httpClient, _, onTokenRefresh))
    }
