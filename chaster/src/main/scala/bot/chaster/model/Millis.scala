package bot.chaster.model

import bot.chaster.model.instances.given

import io.circe.{Decoder, Encoder}
import scala.concurrent.duration.*

object Millis:
  opaque type Millis = FiniteDuration
  given Conversion[Millis, FiniteDuration] = identity
  given Encoder[Millis]                    = Encoder[Long].contramap(_.toMillis)
  given Decoder[Millis]                    = Decoder[Long].map(_.millis)
