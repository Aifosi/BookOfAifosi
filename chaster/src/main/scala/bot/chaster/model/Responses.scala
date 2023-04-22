package bot.chaster.model

import bot.chaster.model.instances.given

import io.circe.Decoder
import scala.concurrent.duration.FiniteDuration

case class VoteResponse(
  duration: FiniteDuration,
) derives Decoder
