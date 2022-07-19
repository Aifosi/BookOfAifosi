package bookofaifosi.model

import java.time.Instant
import java.util.UUID

case class UserToken(
  id: UUID,
  accessToken: String,
  expiresAt: Instant,
  refreshToken: String,
  scope: String,
)
