package bookofaifosi.db

import bookofaifosi.Bot
import doobie.postgres.*
import doobie.postgres.implicits.*
import doobie.syntax.string.*
import bookofaifosi.model.{ChasterID, DiscordID, RegisteredUser, PendingTask as PendingTaskModel}
import bookofaifosi.db.Filters.*
import cats.effect.IO
import doobie.Fragment
import doobie.util.log.LogHandler
import doobie.syntax.connectionio.*
import cats.syntax.traverse.*
import cats.syntax.functor.*

import java.time.Instant
import java.util.UUID

import scala.concurrent.duration.FiniteDuration
import scala.util.chaining.*

private case class PendingTask(
  id: UUID,
  title: String,
  messageID: DiscordID,
  userID: UUID,
  keyholderID: UUID,
  completed: Boolean,
  deadline: Option[Instant],
)

object PendingTaskRepository extends ModelRepository[PendingTask, PendingTaskModel] with Insert[PendingTask]:
  override protected val table: Fragment = fr"pending_tasks"
  override protected val columns: List[String] = List("id", "title", "message_discord_id", "user_id", "keyholder_id", "completed", "deadline")

  override def toModel(pendingTask: PendingTask): IO[PendingTaskModel] =
    for
      user <- RegisteredUserRepository.get(pendingTask.userID.equalID)
      keyholder <- RegisteredUserRepository.get(pendingTask.keyholderID.equalID)
    yield PendingTaskModel(pendingTask.id, pendingTask.title, pendingTask.messageID, user, keyholder, pendingTask.completed, pendingTask.deadline)

  def add(
    title: String,
    messageID: DiscordID,
    user: RegisteredUser,
    keyholders: List[RegisteredUser],
    deadline: Option[Instant],
  ): IO[List[PendingTaskModel]] =
    //keyholders.map(keyholder => (title, messageID, user.id, keyholder.id, deadline)).traverse(insertOne(_)("title", "message_discord_id", "user_id", "keyholder_id", "deadline")).flatMap(_.traverse(toModel))
    insertMany(
      keyholders.map(keyholder => (title, messageID, user.id, keyholder.id, deadline))
    )(
      "title", "message_discord_id", "user_id", "keyholder_id", "deadline"
    )
      .compile
      .toList
      .transact(Bot.postgresTransactor)
      .flatMap(_.traverse(toModel))
