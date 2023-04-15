package bot.db

import bot.db.Filters.*
import bot.db.{ModelRepository, RegisteredUserRepository, given}
import bot.model.{ChasterID, RecentLockHistory as RecentLockHistoryModel, given}
import bot.utils.Maybe
import cats.data.EitherT
import cats.effect.IO
import cats.effect.LiftIO.*
import cats.syntax.option.*
import doobie.postgres.implicits.*
import doobie.syntax.connectionio.*
import doobie.syntax.string.*
import doobie.{Fragment, LogHandler, Transactor}

import java.time.Instant
import java.util.UUID

case class RecentLockHistory(
  userID: UUID,
  lockID: ChasterID,
  mostRecentEventTime: Option[Instant]
)

class RecentLockHistoryRepository(
  registeredUserRepository: RegisteredUserRepository,
)(
  using transactor: Transactor[IO], logHandler: LogHandler
) extends ModelRepository[RecentLockHistory, RecentLockHistoryModel]:
  override protected val table: Fragment = fr"recent_lock_history"
  override protected val columns: List[String] = List("user_id", "lock_id", "most_recent_event_time")

  override def toModel(lockHistory: RecentLockHistory): Maybe[RecentLockHistoryModel] =
    for
      user <- registeredUserRepository.get(lockHistory.userID.equalID).to[Maybe]
    yield RecentLockHistoryModel(user, lockHistory.lockID, lockHistory.mostRecentEventTime)

  def add(
    userID: UUID,
    lockID: ChasterID,
    mostRecentEventTime: Option[Instant],
  ): IO[RecentLockHistoryModel] =
    sql"insert into $table (user_id, lock_id, most_recent_event_time) values ($userID, $lockID, $mostRecentEventTime)"
      .update
      .withUniqueGeneratedKeys[RecentLockHistory]("user_id", "lock_id", "most_recent_event_time")
      .transact(transactor)
      .flatMap(unsafeToModel)

  def update(
    userID: UUID,
    lockID: ChasterID,
    mostRecentEventTime: Option[Instant],
  ): IO[RecentLockHistoryModel] =
    update(
      fr"most_recent_event_time = $mostRecentEventTime".some,
    )(fr"user_id = $userID", fr"lock_id = $lockID")
