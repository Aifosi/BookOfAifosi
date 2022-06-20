package bookofaifosi.model

import java.time.Instant

case class TaskSubscription(
  registeredUser: RegisteredUser,
  user: User,
  lockID: String,
  mostRecentEventTime: Option[Instant],
)
