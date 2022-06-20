package bookofaifosi.model

import java.time.Instant
import bookofaifosi.db.User as DBUser

case class TaskSubscription(
  dbUser: DBUser,
  user: User,
  lockID: String,
  mostRecentEventTime: Option[Instant],
)
