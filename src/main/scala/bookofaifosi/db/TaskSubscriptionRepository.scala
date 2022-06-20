package bookofaifosi.db

import bookofaifosi.Bot
import bookofaifosi.chaster.Client
import bookofaifosi.db.mkFragment
import bookofaifosi.model.{TaskSubscription as TaskSubscriptionModel, User}
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
  id: UUID,
  lockID: String,
  mostRecentEventTime: Option[Instant]
):
  def toModel: IO[TaskSubscriptionModel] =
    for
      maybeDBUser <- UserRepository.find(id = id.some)
      dbUser <- IO.fromOption(maybeDBUser)(new Exception(s"Could not find user with ID: $id"))
      discord <- Bot.discord.get
      user <- discord.userByID(dbUser.discordID)
    yield TaskSubscriptionModel(dbUser, user, lockID, mostRecentEventTime)

object TaskSubscriptionRepository:
  val list: IO[List[TaskSubscriptionModel]] =
    sql"select user_id, lock_id, most_recent_event_time from task_subscriptions"
      .queryWithLogHandler[TaskSubscription](LogHandler.jdkLogHandler)
      .to[List]
      .transact(Bot.xa)
      .flatMap(_.traverse(_.toModel))

  def add(
    userID: UUID,
    lockID: String,
    mostRecentEventTime: Option[Instant],
  ): IO[TaskSubscriptionModel] =
    sql"insert into task_subscriptions(user_id, lock_id, most_recent_event_time) values ($userID, $lockID, $mostRecentEventTime)"
      .updateWithLogHandler(LogHandler.jdkLogHandler)
      .withUniqueGeneratedKeys[TaskSubscription]("user_id", "lock_id", "most_recent_event_time")
      .transact(Bot.xa)
      .flatMap(_.toModel)

  def update(
    userID: UUID,
    lockID: String,
    mostRecentEventTime: Option[Instant],
  ): IO[TaskSubscriptionModel] =
    sql"update task_subscriptions set most_recent_event_time = $mostRecentEventTime where user_id = $userID and lock_id = $lockID"
      .updateWithLogHandler(LogHandler.jdkLogHandler)
      .withUniqueGeneratedKeys[TaskSubscription]("user_id", "lock_id", "most_recent_event_time")
      .transact(Bot.xa)
      .flatMap(_.toModel)
