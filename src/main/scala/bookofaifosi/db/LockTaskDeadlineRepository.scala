package bookofaifosi.db

import bookofaifosi.Bot
import bookofaifosi.model.{ChasterID, LockTaskDeadline as LockTaskDeadlineModel}
import bookofaifosi.db.Filters.*
import cats.effect.IO
import cats.syntax.functor.*
import cats.syntax.traverse.*
import doobie.syntax.string.*
import doobie.postgres.implicits.*
import doobie.syntax.connectionio.*
import doobie.util.log.LogHandler
import doobie.{ConnectionIO, Fragment}

import java.time.Instant
import java.util.UUID
import scala.concurrent.duration.FiniteDuration

case class LockTaskDeadline(
  lockID: ChasterID,
  keyholderID: UUID,
  userID: UUID,
  deadline: FiniteDuration,
  mostRecentEventTime: Option[Instant],
)

object LockTaskDeadlineRepository extends ModelRepository[LockTaskDeadline, LockTaskDeadlineModel]:
  override protected val table: Fragment = fr"lock_task_deadlines"
  override protected val selectColumns: Fragment = fr"lock_id, keyholder_id, user_id, deadline, most_recent_event_time"
  override def toModel(lockTaskDeadline: LockTaskDeadline): IO[LockTaskDeadlineModel] =
    for
      keyholder <- RegisteredUserRepository.get(lockTaskDeadline.keyholderID.equalID)
      user <- RegisteredUserRepository.get(lockTaskDeadline.userID.equalID)
    yield LockTaskDeadlineModel(lockTaskDeadline.lockID, keyholder, user, lockTaskDeadline.deadline, lockTaskDeadline.mostRecentEventTime)
  def add(
    lockID: ChasterID,
    keyholderID: UUID,
    userID: UUID,
    deadline: FiniteDuration,
    mostRecentEventTime: Option[Instant],
  ): IO[LockTaskDeadlineModel] =
    sql"insert into $table (lock_id, keyholder_id, user_id, deadline, most_recent_event_time) values ($lockID, $keyholderID, $userID, $deadline, $mostRecentEventTime)"
      .update
      .withUniqueGeneratedKeys[LockTaskDeadline]("lock_id", "keyholder_id", "user_id", "deadline", "most_recent_event_time")
      .transact(Bot.xa)
      .flatMap(toModel)

  def update(
    lockID: ChasterID,
    keyholderID: UUID,
    deadline: FiniteDuration,
    mostRecentEventTime: Option[Instant],
  ): IO[LockTaskDeadlineModel] =
    sql"update lock_task_deadlines set most_recent_event_time = $mostRecentEventTime, deadline = $deadline, $updatedAt where lock_id = $lockID and keyholder_id = $keyholderID"
      .update
      .withUniqueGeneratedKeys[LockTaskDeadline]("lock_id", "keyholder_id", "user_id", "deadline", "most_recent_event_time")
      .transact(Bot.xa)
      .flatMap(toModel)
