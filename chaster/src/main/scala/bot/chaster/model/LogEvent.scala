package bot.chaster.model

import bot.chaster.model.instances.{*, given}
import bot.model.ChasterID

import io.circe.{Decoder, Encoder, HCursor, Json}
import io.circe.syntax.*
import java.time.Instant
import scala.concurrent.duration.*
import scala.util.Try

case class Event[T: Decoder](
  extension: Option[String],
  _id: ChasterID,
  `type`: String,
  role: String,
  description: String,
  createdAt: Instant,
  user: Option[User],
  payload: T,
) extends WithID:
  def as[TT: Decoder](using ev: T =:= Json): Option[Event[TT]] =
    ev(payload).as[TT].toOption.map(Event(extension, _id, `type`, role, description, createdAt, user, _))

object Event:
  inline given decoder[T: Decoder]: Decoder[Event[T]] = (c: HCursor) =>
    for
      extension   <- c.get[Option[String]]("extension")
      id          <- c.get[ChasterID]("_id")
      `type`      <- c.get[String]("type")
      role        <- c.get[String]("role")
      description <- c.get[String]("description")
      createdAt   <- c.get[Instant]("createdAt")
      user        <- c.get[Option[User]]("user")
      payload     <- c.get[T]("payload")
    yield Event(extension, id, `type`, role, description, createdAt, user, payload)

enum SegmentType:
  case AddTime, RemoveTime, AddRemoveTime, Text, SetFreeze, SetUnfreeze, Pillory, Freeze

object SegmentType:
  given Decoder[SegmentType] = Decoder[String].emapTry { string =>
    Try(SegmentType.valueOf(string.kebabToPascalCase))
  }
  given Encoder[SegmentType] = Encoder[String].contramap(_.toString.pascalToKebabCase)

case class Segment(
  text: String,
  `type`: SegmentType = SegmentType.Text,
  duration: FiniteDuration = 1.hour,
) derives Decoder, Encoder.AsObject

case class WheelTurnedPayload(
  segment: Segment,
) derives Decoder
