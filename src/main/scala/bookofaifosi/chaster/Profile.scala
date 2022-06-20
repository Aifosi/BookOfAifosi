package bookofaifosi.chaster

import io.circe.Decoder

case class Profile (
  _id: String,
  username: String,
  gender: String,
  role: String,
  discordId: String,
  discordUsername: String,
) derives Decoder