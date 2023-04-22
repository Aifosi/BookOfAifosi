package bot.db

import bot.Bot
import bot.db.{Insert, ModelRepository, RegisteredUserRepository}
import bot.db.Filters.*
import bot.db.given
import bot.model.{ChasterID, Discord, DiscordID, RegisteredUser}
import bot.model.PendingTask as PendingTaskModel
import bot.syntax.io.*
import bot.utils.Maybe

import cats.data.EitherT
import cats.effect.{Deferred, IO}
import cats.syntax.functor.*
import cats.syntax.traverse.*
import doobie.{Fragment, LogHandler, Transactor}
import doobie.postgres.*
import doobie.postgres.implicits.*
import doobie.syntax.connectionio.*
import doobie.syntax.string.*
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

class PendingTaskRepository(
  registeredUserRepository: RegisteredUserRepository,
)(using
  Transactor[IO],
  LogHandler,
) extends ModelRepository[PendingTask, PendingTaskModel]:
  override protected val table: Fragment       = fr"pending_tasks"
  override protected val columns: List[String] =
    List("id", "title", "message_discord_id", "user_id", "keyholder_id", "completed", "deadline")

  override def toModel(pendingTask: PendingTask): Maybe[PendingTaskModel] =
    for
      user      <- registeredUserRepository.get(pendingTask.userID.equalID).to[Maybe]
      keyholder <- registeredUserRepository.get(pendingTask.keyholderID.equalID).to[Maybe]
    yield PendingTaskModel(
      pendingTask.id,
      pendingTask.title,
      pendingTask.messageID,
      user,
      keyholder,
      pendingTask.completed,
      pendingTask.deadline,
    )

  def add(
    title: String,
    messageID: DiscordID,
    user: RegisteredUser,
    keyholder: RegisteredUser,
    deadline: Option[Instant],
  ): IO[PendingTaskModel] =
    insertOne(
      (title, messageID, user.id, keyholder.id, deadline),
    )(
      "title",
      "message_discord_id",
      "user_id",
      "keyholder_id",
      "deadline",
    )
      .flatMap(unsafeToModel)
