package bot.chaster.model

import io.circe.Decoder
import java.time.Instant

case class AccessToken(
  access_token: String,
  expires_in: Int,
  refresh_expires_in: Int,
  refresh_token: String,
  token_type: String,
  scope: String,
) derives Decoder:
  val expiresAt: Instant = Instant.now().plusSeconds(expires_in)
