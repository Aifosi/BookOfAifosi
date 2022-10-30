package lurch.commands

import bot.Bot
import bot.commands.{Hidden, NoLog, ReactionCommand}
import bot.db.Filters.*
import bot.db.RegisteredUserRepository
import bot.model.event.ReactionEvent
import bot.model.{Message, RegisteredUser, User}
import bot.syntax.io.*
import cats.effect.IO
import cats.syntax.option.*
import doobie.syntax.string.*
import lurch.db.PendingTaskRepository
import lurch.model.PendingTask
import org.typelevel.log4cats.Logger

import scala.util.matching.Regex

object TaskCompleter extends ReactionCommand with Hidden with NoLog:
  override val pattern: String = "✅"

  private def handleTask(task: PendingTask, user: User)(using Logger[IO]): IO[Unit] =
    user.discordID match {
      case task.user.discordID =>
        for
          _ <- PendingTaskRepository.update(fr"completed = TRUE".some)(task.id.equalID.get)
          _ <- task.user.sendMessage(s"${task.title} completed")
          maybeJumpUrl <- task.message.map(_.jumpUrl).value
          _ <- task.keyholder.sendMessage(s"${task.user.mention} completed $task${maybeJumpUrl.fold("")("\n" + _)}")
        yield ()
      case _ if !task.completed =>
        for
          _ <- task.message.semiflatMap(_.removeUserReaction(pattern, task.keyholder).logErrorOption).value
          _ <- task.keyholder.sendMessage(s"${user.mention} has not completed the task yet!")
        yield ()
      case _ =>
        for
          _ <- PendingTaskRepository.remove(task.id.equalID)
          _ <- task.message.semiflatMap(_.delete).value
          _ <- task.user.sendMessage(s"Task \"$task\" completion verified by ${task.keyholder.mention}")
        yield ()
    }

  override def apply(pattern: String, event: ReactionEvent)(using Logger[IO]): IO[Boolean] =
    for
      pendingTasks <- PendingTaskRepository.list(event.messageID.equalMessageID)
      maybeTask = pendingTasks.find(task => task.user.discordID == event.author.discordID || task.keyholder.discordID == event.author.discordID)
      _ <- maybeTask.fold(IO.unit)(handleTask(_, event.author))
    yield maybeTask.isDefined

  override val description: String = "Checks for reactions on task messages"
