package bot.chaster.model

import bot.chaster.model.instances.given

import io.circe.{Decoder, Encoder}
import scala.concurrent.duration.FiniteDuration

case class FrequencyParam(
  nbActions: Int,
  frequency: FiniteDuration,
) derives Decoder, Encoder.AsObject

case class TimeLimitParam(
  timeLimit: FiniteDuration,
) derives Decoder, Encoder.AsObject

case class DurationParam(
  duration: FiniteDuration,
) derives Decoder, Encoder.AsObject
