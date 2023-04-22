package bot.chaster.model

import bot.model.ChasterID

import io.circe.Decoder

case class User(
  _id: ChasterID,
  username: String,
  gender: Option[String],
  role: String,
  discordId: Option[String],
  discordUsername: Option[String],
) extends WithID
    derives Decoder

case class PublicUser(
  _id: ChasterID,
  username: String,
  gender: Option[String],
  isDisabled: Boolean,
  discordId: Option[String],
  discordUsername: Option[String],
) extends WithID
    derives Decoder
