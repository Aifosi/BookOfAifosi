package lurch.wheel

import bot.chaster.{Lock, Segment}
import bot.model.{ChasterID, Message, RegisteredUser}
import bot.syntax.io.*
import bot.chaster.Client.{*, given}
import bot.db.{RegisteredUserRepository, given}
import bot.tasks.{TextWheelCommand, keyholder}
import cats.data.{OptionT, EitherT}
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

  override def run(user: RegisteredUser, lock: Lock, text: String)(using Logger[IO]): IO[Boolean] =
    def addReaction(message: Message, task: Task): IO[Unit] =
      val either = for
        keyholder <- keyholder(lock).toRight(s"Unable to find keyholder for lock ${lock._id}.")
        _ <- EitherT.liftF(PendingTaskRepository.add(task.title, message.id, user, keyholder, None))
        _ <- EitherT.liftF(message.addReaction(TaskCompleter.pattern))
      yield ()
      either.foldF(
        error => Logger[IO].warn(error),
        IO.pure
      )

    handleTask(text, user, addReaction).value.as(true)

