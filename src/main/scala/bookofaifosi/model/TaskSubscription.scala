package bookofaifosi.model

import java.time.Instant

case class TaskSubscription(
  registeredUser: RegisteredUser,
  user: User,
  lockID: ChasterID,
  mostRecentEventTime: Option[Instant],
)
