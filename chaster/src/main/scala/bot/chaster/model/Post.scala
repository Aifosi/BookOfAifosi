package bot.chaster.model

import bot.model.ChasterID

import io.circe.Decoder
import java.time.Instant

case class Data(
  voteEndsAt: Instant,
  createdAt: Option[Instant],
) derives Decoder

case class Post(
  _id: ChasterID,
  lock: Lock,
  `type`: String,
  user: User,
  data: Data,
) extends WithID
    derives Decoder
