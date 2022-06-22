package bookofaifosi.model

import java.time.Instant
import scala.concurrent.duration.FiniteDuration

case class LockTaskDeadline(
  lockID: String,
  keyholder: RegisteredUser,
  user: RegisteredUser,
  deadline: FiniteDuration,
  mostRecentEventTime: Option[Instant],
)
