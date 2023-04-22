package bot.chaster.model

import io.circe.Encoder

case class KeyholderLockSearch(
  status: String,
  name: String,
) derives Encoder.AsObject
