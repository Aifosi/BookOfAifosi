package bookofaifosi.db

import doobie.postgres.implicits.*
import doobie.syntax.string.*
import bookofaifosi.model.RegisteredUser
import doobie.Fragment

import java.time.Instant
import java.util.UUID

private case class PendingTask(
  id: UUID,
  userID: UUID,
  keyholderID: UUID,
  deadline: Instant,
)

object PendingTaskRepository extends Repository[PendingTask]:
  override protected val selectAll: Fragment = fr"select id, user_id, keyholder_id, deadline from pending_tasks"
