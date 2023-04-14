package bot.model

import bot.model.{ChasterID, RegisteredUser}

import java.time.Instant
import scala.concurrent.duration.FiniteDuration

case class LockTaskDeadline(
  lockID: ChasterID,
  keyholder: RegisteredUser,
  user: RegisteredUser,
  deadline: FiniteDuration,
  mostRecentEventTime: Option[Instant],
)
