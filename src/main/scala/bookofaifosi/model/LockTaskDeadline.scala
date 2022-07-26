package bookofaifosi.model

import java.time.Instant
import scala.concurrent.duration.FiniteDuration

case class LockTaskDeadline(
  lockID: ChasterID,
  keyholder: RegisteredUser,
  user: RegisteredUser,
  deadline: FiniteDuration,
  mostRecentEventTime: Option[Instant],
)
