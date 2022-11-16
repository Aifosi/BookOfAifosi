package lurch.wheel

import bot.chaster.Segment
import bot.model.{ChasterID, Message, RegisteredUser}
import bot.syntax.io.*
import bot.chaster.Client.{*, given}
import bot.db.given
import bot.tasks.TextWheelCommand
import cats.data.OptionT
import cats.effect.IO
import cats.syntax.functor.*
import lurch.Lurch
import lurch.commands.{Task, TaskCompleter}
import lurch.db.PendingTaskRepository
import org.typelevel.log4cats.Logger
import doobie.implicits.*

import scala.util.matching.Regex

object Task extends TextWheelCommand:
  override def pattern: Regex = "Task: (.+)".r

  def handleTask(task: String, user: RegisteredUser, andAlso: (Message, Task) => IO[Unit] = (_, _) => IO.unit)(using Logger[IO]): OptionT[IO, String] =
    OptionT(fr"call GetTask(${user.discordID}, $task)".query[Task].option.transact(Lurch.mysql.transactor))
      .flatTapNone(Logger[IO].warn(s"Unable to get task for ${user.discordID}, $task"))
      .map(_.cleanedDescription)
      .flatMap { task =>
        Lurch.channels.tortureChamber.sendMessage(s"${user.mention} rolled task ${task.id} - ${task.title}")
          .semiflatMap(andAlso(_, task))
          .as(s"Rolled task ${task.id} - ${task.title}\n${task.description}")
          .semiflatTap(user.sendMessage)
      }

  override def run(user: RegisteredUser, lockID: ChasterID, text: String)(using Logger[IO]): IO[Boolean] =
    def addReaction(message: Message, task: Task): IO[Unit] =
      for
        registeredKeyholders <- user.registeredKeyholders
        _ <- PendingTaskRepository.add(task.title, message.id, user, registeredKeyholders, None)
        _ <- message.addReaction(TaskCompleter.pattern)
      yield ()

    handleTask(text, user, addReaction).value.as(true)

