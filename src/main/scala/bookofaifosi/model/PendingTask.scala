package bookofaifosi.model

import java.time.Instant
import java.util.UUID

case class PendingTask(
  id: UUID,
  task: String,
  user: RegisteredUser,
  keyholder: RegisteredUser,
  deadline: Instant,
)
