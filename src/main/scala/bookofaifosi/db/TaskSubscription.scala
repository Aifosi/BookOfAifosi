package bookofaifosi.db

import bookofaifosi.Bot
import bookofaifosi.chaster.Client
import bookofaifosi.db.mkFragment
import bookofaifosi.model.User
import bookofaifosi.db.User as DBUser
import bookofaifosi.db.UserRepository
import cats.effect.IO
import doobie.{ConnectionIO, Fragment}
import doobie.syntax.string.*
import doobie.syntax.connectionio.*
import doobie.postgres.implicits.*
import cats.syntax.functor.*
import cats.syntax.traverse.*
import cats.syntax.option.*
import doobie.util.log.LogHandler

import java.time.Instant
import java.util.UUID

case class TaskSubscription(
  dbUser: DBUser,
  user: User,
  lockID: String,
  mostRecentEventTime: Option[Instant]
)

case class TaskSubscriptionDB(
  id: UUID,
  lockID: String,
  mostRecentEventTime: Option[Instant]
):
  def toModel: IO[TaskSubscription] =
    for
      maybeDBUser <- UserRepository.find(id = id.some).transact(Bot.xa)
      dbUser <- IO.fromOption(maybeDBUser)(new Exception(s"Could not find user with ID: $id"))
      discord <- Bot.discord.get
      user <- discord.userByID(dbUser.discordID)
    yield TaskSubscription(dbUser, user, lockID, mostRecentEventTime)

object TaskSubscriptionRepository:
  val list: IO[List[TaskSubscription]] =
    sql"select user_id, lock_id, most_recent_event_time from task_subscriptions"
      .queryWithLogHandler[TaskSubscriptionDB](LogHandler.jdkLogHandler)
      .to[List]
      .transact(Bot.xa)
      .flatMap(_.traverse(_.toModel))

  def add(
    userID: UUID,
    lockID: String,
    mostRecentEventTime: Option[Instant],
  ): IO[TaskSubscription] =
    sql"insert into task_subscriptions(user_id, lock_id, most_recent_event_time) values ($userID, $lockID, $mostRecentEventTime)"
      .updateWithLogHandler(LogHandler.jdkLogHandler)
      .withUniqueGeneratedKeys[TaskSubscriptionDB]("user_id", "lock_id", "most_recent_event_time")
      .transact(Bot.xa)
      .flatMap(_.toModel)

  def update(
    userID: UUID,
    lockID: String,
    mostRecentEventTime: Option[Instant],
  ): IO[TaskSubscription] =
    sql"update task_subscriptions set most_recent_event_time = $mostRecentEventTime where user_id = $userID and lock_id = $lockID"
      .updateWithLogHandler(LogHandler.jdkLogHandler)
      .withUniqueGeneratedKeys[TaskSubscriptionDB]("user_id", "lock_id", "most_recent_event_time")
      .transact(Bot.xa)
      .flatMap(_.toModel)
