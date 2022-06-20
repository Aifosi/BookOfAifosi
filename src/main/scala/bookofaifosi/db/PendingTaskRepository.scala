package bookofaifosi.db

import bookofaifosi.db.User as DBUser

import java.time.Instant
import java.util.UUID

case class PendingTask(
  id: UUID,
  user: DBUser,
  keyholder: DBUser,
  deadline: Instant,
)

private case class PendingTaskDB(
  id: UUID,
  userID: UUID,
  keyholderID: UUID,
  deadline: Instant,
)

object PendingTaskRepository {

}
