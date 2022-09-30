package bookofaifosi.model

import java.time.Instant

case class RecentLockHistory(
  registeredUser: RegisteredUser,
  lockID: ChasterID,
  mostRecentEventTime: Option[Instant],
)
