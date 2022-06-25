package bookofaifosi.db

import bookofaifosi.Bot
import doobie.postgres.implicits.*
import doobie.syntax.string.*
import bookofaifosi.model.PendingTask as PendingTaskModel
import bookofaifosi.db.Filters.*
import bookofaifosi.db.LockTaskDeadlineRepository.toModel
import cats.effect.IO
import doobie.Fragment
import doobie.util.log.LogHandler
import doobie.syntax.connectionio.*
import java.time.Instant
import java.util.UUID
import cats.syntax.functor.*
import scala.concurrent.duration.FiniteDuration

private case class PendingTask(
  id: UUID,
  task: String,
  userID: UUID,
  keyholderID: UUID,
  deadline: Instant,
)

object PendingTaskRepository extends ModelRepository[PendingTask, PendingTaskModel]:
  override protected val table: Fragment = fr"pending_tasks"
  override protected val selectColumns: Fragment = fr"id, task, user_id, keyholder_id, deadline"

  override def toModel(pendingTask: PendingTask): IO[PendingTaskModel] =
    for
      user <- RegisteredUserRepository.get(pendingTask.userID.equalID)
      keyholder <- RegisteredUserRepository.get(pendingTask.keyholderID.equalID)
    yield PendingTaskModel(pendingTask.id, pendingTask.task, user, keyholder, pendingTask.deadline)

  def add(
    task: String,
    userID: UUID,
    keyholderID: UUID,
    deadline: Instant,
  ): IO[PendingTaskModel] =
    sql"insert into pending_tasks(task, user_id, keyholder_id, deadline) values ($task, $userID, $keyholderID, $deadline)"
      .update
      .withUniqueGeneratedKeys[PendingTask]("id", "task", "user_id", "keyholder_id", "deadline")
      .transact(Bot.xa)
      .flatMap(toModel)
