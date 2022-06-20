package bookofaifosi.model

import bookofaifosi.wrappers.User
import bookofaifosi.db.User as DBUser
import java.time.Instant

case class TaskSubscription(
  dbUser: DBUser,
  user: User,
  taskID: String,
  mostRecentEventTime: Option[Instant]
)
