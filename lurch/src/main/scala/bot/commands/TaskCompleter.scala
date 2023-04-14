package bot.commands

import bot.Bot
import bot.commands.{Hidden, NoLog, ReactionCommand}
import bot.db.Filters.*
import bot.db.RegisteredUserRepository
import bot.model.event.ReactionEvent
import bot.model.{Channel, Message, RegisteredUser, User}
import bot.syntax.io.*
import cats.effect.IO
import cats.effect.kernel.Deferred
import cats.syntax.option.*
import doobie.syntax.string.*
import bot.db.PendingTaskRepository
import bot.model.PendingTask
import org.typelevel.log4cats.Logger

import scala.util.matching.Regex

class TaskCompleter(
  pendingTaskRepository: PendingTaskRepository,
  tortureChamberChannel: Deferred[IO, Option[Channel]],
) extends ReactionCommand with Hidden with NoLog:
  override val pattern: String = TaskCompleter.pattern

  private def handleTask(task: PendingTask, user: User)(using Logger[IO]): IO[Unit] =
    user.discordID match {
      case task.user.discordID =>
        for
          _ <- pendingTaskRepository.update(fr"completed = TRUE".some)(task.id.equalID.get)
          _ <- task.user.sendMessage(s"${task.title} completed")
          maybeJumpUrl <- task.message(tortureChamberChannel).map(_.jumpUrl).value
          _ <- task.keyholder.sendMessage(s"${task.user.mention} completed $task${maybeJumpUrl.fold("")("\n" + _)}")
        yield ()
      case _ if !task.completed =>
        for
          _ <- task.message(tortureChamberChannel).semiflatMap(_.removeUserReaction(pattern, task.keyholder).logErrorOption).value
          _ <- task.keyholder.sendMessage(s"${user.mention} has not completed the task yet!")
        yield ()
      case _ =>
        for
          _ <- pendingTaskRepository.remove(task.id.equalID)
          _ <- task.message(tortureChamberChannel).semiflatMap(_.delete).value
          _ <- task.user.sendMessage(s"Task \"$task\" completion verified by ${task.keyholder.mention}")
        yield ()
    }

  override def apply(pattern: String, event: ReactionEvent)(using Logger[IO]): IO[Boolean] =
    for
      pendingTasks <- pendingTaskRepository.list(event.messageID.equalMessageID)
      maybeTask = pendingTasks.find(task => task.user.discordID == event.author.discordID || task.keyholder.discordID == event.author.discordID)
      _ <- maybeTask.fold(IO.unit)(handleTask(_, event.author))
    yield maybeTask.isDefined

object TaskCompleter:
  val pattern: String = "✅"
