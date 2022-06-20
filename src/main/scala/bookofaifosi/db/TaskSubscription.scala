package bookofaifosi.db

import bookofaifosi.Bot
import bookofaifosi.chaster.Client
import bookofaifosi.db.mkFragment
import bookofaifosi.wrappers.User
import bookofaifosi.db.User as DBUser
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

case class TaskSubscription(
  dbUser: DBUser,
  user: User,
  lockID: String,
  mostRecentEventTime: Option[Instant]
):
  def updateMostRecentEventTime(mostRecentEventTime: Instant): IO[TaskSubscription] =
    TaskSubscription.update(dbUser.discordID, lockID, mostRecentEventTime.some)

case class TaskSubscriptionDB(
  discordID: Long,
  lockID: String,
  mostRecentEventTime: Option[Instant]
):
  def toModel: IO[TaskSubscription] =
    for
      maybeDBUser <- DBUser.find(discordID = discordID.some).transact(Bot.xa)
      dbUser <- IO.fromOption(maybeDBUser)(new Exception(s"Could not find user with discord ID: $discordID"))
      discord <- Bot.discord.get
      user <- discord.userByID(discordID)
    yield TaskSubscription(dbUser, user, lockID, mostRecentEventTime)

object TaskSubscription:
  val list: IO[List[TaskSubscription]] =
    sql"select discord_id, lock_id, most_recent_event_time from task_subscriptions"
      .queryWithLogHandler[TaskSubscriptionDB](LogHandler.jdkLogHandler)
      .to[List]
      .transact(Bot.xa)
      .flatMap(_.traverse(_.toModel))

  def add(
    discordID: Long,
    taskID: String,
    mostRecentEventTime: Option[Instant],
  ): IO[TaskSubscription] =
    sql"insert into task_subscriptions(discord_id, lock_id, most_recent_event_time) values ($discordID, $taskID, $mostRecentEventTime)"
      .updateWithLogHandler(LogHandler.jdkLogHandler)
      .withUniqueGeneratedKeys[TaskSubscriptionDB]("discord_id", "lock_id", "most_recent_event_time")
      .transact(Bot.xa)
      .flatMap(_.toModel)

  def update(
    discordID: Long,
    taskID: String,
    mostRecentEventTime: Option[Instant],
  ): IO[TaskSubscription] =
    sql"update task_subscriptions set most_recent_event_time = $mostRecentEventTime where discord_id = $discordID and lock_id = $taskID"
      .updateWithLogHandler(LogHandler.jdkLogHandler)
      .withUniqueGeneratedKeys[TaskSubscriptionDB]("discord_id", "lock_id", "most_recent_event_time")
      .transact(Bot.xa)
      .flatMap(_.toModel)
