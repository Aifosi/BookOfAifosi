package bookofaifosi.db

import bookofaifosi.Bot
import bookofaifosi.chaster.Client
import bookofaifosi.db.mkFragment
import bookofaifosi.db.Filters.*
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
)

object TaskSubscriptionRepository extends ModelRepository[TaskSubscription, TaskSubscriptionModel]:
  override protected val selectAll: Fragment = fr"select user_id, lock_id, most_recent_event_time from task_subscriptions"

  override def toModel(taskSubscription: TaskSubscription): IO[TaskSubscriptionModel] =
    for
      user <- RegisteredUserRepository.get(taskSubscription.id.equalID)
      discord <- Bot.discord.get
      discordUser <- discord.userByID(user.discordID)
    yield TaskSubscriptionModel(user, discordUser, taskSubscription.lockID, taskSubscription.mostRecentEventTime)

  def add(
    userID: UUID,
    lockID: String,
    mostRecentEventTime: Option[Instant],
  ): IO[TaskSubscriptionModel] =
    sql"insert into task_subscriptions(user_id, lock_id, most_recent_event_time) values ($userID, $lockID, $mostRecentEventTime)"
      .updateWithLogHandler(Log.handler)
      .withUniqueGeneratedKeys[TaskSubscription]("user_id", "lock_id", "most_recent_event_time")
      .transact(Bot.xa)
      .flatMap(toModel)

  def update(
    userID: UUID,
    lockID: String,
    mostRecentEventTime: Option[Instant],
  ): IO[TaskSubscriptionModel] =
    sql"update task_subscriptions set most_recent_event_time = $mostRecentEventTime where user_id = $userID and lock_id = $lockID"
      .updateWithLogHandler(Log.handler)
      .withUniqueGeneratedKeys[TaskSubscription]("user_id", "lock_id", "most_recent_event_time")
      .transact(Bot.xa)
      .flatMap(toModel)
