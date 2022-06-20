package bookofaifosi.chaster

import bookofaifosi.{Bot, Registration}
import bookofaifosi.db.User as DBUser
import bookofaifosi.chaster.*
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
import io.circe.{Decoder, Encoder, HCursor}
import org.http4s.client.Client

import scala.concurrent.duration.*
import java.util.UUID
import scala.deriving.Mirror

trait Paged:
  def lastId: Option[String]
  def limit: Option[Int]

case class NoBody(
  lastId: Option[String] = None,
  limit: Option[Int] = 50.some,
) extends Paged

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

object Paged:
  inline given encoder[A <: Paged: Mirror.Of]: Encoder[A] = (a: A) =>
    Encoder.AsObject.derived[A]
      .mapJsonObject(_.add("limit", a.limit.asJson).add("lastId", a.lastId.asJson))
      .apply(a)

  extension (client: Client[IO])
    def getAll[A <: WithID: Decoder](uri: Uri, headers: Header.ToRaw*)(lastIDSeen: Option[String]): Stream[IO, A] =
      Stream.unfoldEval(client.expect[Result[A]](POST.apply(NoBody(None), uri, headers*)).map(_ -> true)) { result =>
        result.map {
          case (result, false) => None
          case (result, true) =>
            val continue = result.hasMore && lastIDSeen.forall(lastIDSeen => result.results.forall(_._id != lastIDSeen))
            val lastID = result.results.last._id
            val next = client.expect[Result[A]](POST(NoBody(lastID.some).asJson, uri, headers*))
            (result.results, next.map(_ -> continue)).some
        }
      }.metered(60.seconds / 200).flatMap { results =>
        val lastSeen = for
          lastIDSeen <- lastIDSeen
          lastSeen <- results.find(_._id == lastIDSeen)
        yield results.indexOf(lastSeen)
        Stream.emits(lastSeen.fold(results)(results.slice(0, _)))
      }