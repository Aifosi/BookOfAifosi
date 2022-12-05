package shared.db

import bot.Bot
import bot.db.Filters.*
import bot.db.given
import bot.db.Log.given
import bot.db.{ModelRepository, RegisteredUserRepository}
import bot.model.ChasterID
import cats.effect.IO
import cats.syntax.option.*
import doobie.Fragment
import doobie.postgres.implicits.*
import doobie.syntax.connectionio.*
import doobie.syntax.string.*
import shared.model.RecentLockHistory as RecentLockHistoryModel

import java.time.Instant
import java.util.UUID

case class RecentLockHistory(
  userID: UUID,
  lockID: ChasterID,
  mostRecentEventTime: Option[Instant]
)

object RecentLockHistoryRepository extends ModelRepository[RecentLockHistory, RecentLockHistoryModel]:
  override protected val table: Fragment = fr"recent_lock_history"
  override protected val columns: List[String] = List("user_id", "lock_id", "most_recent_event_time")

  override def toModel(lockHistory: RecentLockHistory): IO[RecentLockHistoryModel] =
    for
      user <- RegisteredUserRepository.get(lockHistory.userID.equalID)
    yield RecentLockHistoryModel(user, lockHistory.lockID, lockHistory.mostRecentEventTime)

  def add(
    userID: UUID,
    lockID: ChasterID,
    mostRecentEventTime: Option[Instant],
  ): IO[RecentLockHistoryModel] =
    sql"insert into $table (user_id, lock_id, most_recent_event_time) values ($userID, $lockID, $mostRecentEventTime)"
      .update
      .withUniqueGeneratedKeys[RecentLockHistory]("user_id", "lock_id", "most_recent_event_time")
      .transact(Bot.postgres.transactor)
      .flatMap(toModel)

  def update(
    userID: UUID,
    lockID: ChasterID,
    mostRecentEventTime: Option[Instant],
  ): IO[RecentLockHistoryModel] =
    update(
      fr"most_recent_event_time = $mostRecentEventTime".some,
    )(fr"user_id = $userID", fr"lock_id = $lockID")
