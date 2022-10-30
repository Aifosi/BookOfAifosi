package bot.model

import java.time.Instant
import java.util.UUID

case class UserToken(
  id: UUID,
  accessToken: String,
  expiresAt: Instant,
  refreshToken: String,
  scope: String,
)

object UserToken:
  //Useful to do request that do not require authentication
  lazy val empty: UserToken = UserToken(UUID.randomUUID(), "", Instant.MAX, "", "")
