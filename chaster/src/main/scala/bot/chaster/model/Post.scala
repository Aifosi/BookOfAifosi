package bot.chaster.model

import bot.model.ChasterID

import io.circe.Decoder
import java.time.Instant

case class ExtensionParty(
  _id: ChasterID,
) extends WithID derives Decoder

case class Data(
  _id: ChasterID,
  voteEndsAt: Instant,
  createdAt: Option[Instant],
) extends WithID derives Decoder

case class Post(
  _id: ChasterID,
  lock: Lock,
  `type`: String,
  user: User,
  data: Data,
  extensionParty: ExtensionParty
) extends WithID
    derives Decoder
