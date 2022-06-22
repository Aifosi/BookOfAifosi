package bookofaifosi.db

import bookofaifosi.Bot
import bookofaifosi.model.LockTaskDeadline as LockTaskDeadlineModel
import bookofaifosi.db.Filters.*
import cats.effect.IO
import doobie.syntax.string.*
import doobie.postgres.implicits.*
import cats.syntax.functor.*
import cats.syntax.traverse.*
import doobie.syntax.connectionio.*
import doobie.util.log.LogHandler
import doobie.{ConnectionIO, Fragment}

import java.time.Instant
import java.util.UUID
import scala.concurrent.duration.FiniteDuration

case class LockTaskDeadline(
  lockID: String,
  keyholderID: UUID,
  userID: UUID,
  deadline: FiniteDuration,
  mostRecentEventTime: Option[Instant],
)

object LockTaskDeadlineRepository extends ModelRepository[LockTaskDeadline, LockTaskDeadlineModel]:
  override protected val selectAll: Fragment = fr"select lock_id, keyholder_id, user_id, deadline, most_recent_event_time from lock_task_deadlines"
  override def toModel(lockTaskDeadline: LockTaskDeadline): IO[LockTaskDeadlineModel] =
    for
      keyholder <- RegisteredUserRepository.get(lockTaskDeadline.keyholderID.equalID)
      user <- RegisteredUserRepository.get(lockTaskDeadline.userID.equalID)
    yield LockTaskDeadlineModel(lockTaskDeadline.lockID, keyholder, user, lockTaskDeadline.deadline, lockTaskDeadline.mostRecentEventTime)

  def add(
    lockID: String,
    keyholderID: UUID,
    userID: UUID,
    deadline: FiniteDuration,
    mostRecentEventTime: Option[Instant],
  ): IO[LockTaskDeadlineModel] =
    sql"insert into lock_task_deadlines(lock_id, keyholder_id, user_id, deadline, most_recent_event_time) values ($lockID, $keyholderID, $userID, $deadline, $mostRecentEventTime)"
      .updateWithLogHandler(Log.handler)
      .withUniqueGeneratedKeys[LockTaskDeadline]("lock_id", "keyholder_id", "user_id", "deadline", "most_recent_event_time")
      .transact(Bot.xa)
      .flatMap(toModel)

  def remove(
    lockID: String,
    keyholderID: UUID,
  ): IO[Unit] =
    sql"delete from lock_task_deadlines where lock_id = $lockID and keyholder_id = $keyholderID"
      .updateWithLogHandler(Log.handler)
      .run
      .void
      .transact(Bot.xa)

  def update(
    lockID: String,
    keyholderID: UUID,
    deadline: FiniteDuration,
    mostRecentEventTime: Option[Instant],
  ): IO[LockTaskDeadlineModel] =
    sql"update lock_task_deadlines set most_recent_event_time = $mostRecentEventTime, deadline = $deadline where lock_id = $lockID and keyholder_id = $keyholderID"
      .updateWithLogHandler(Log.handler)
      .withUniqueGeneratedKeys[LockTaskDeadline]("lock_id", "keyholder_id", "user_id", "deadline", "most_recent_event_time")
      .transact(Bot.xa)
      .flatMap(toModel)