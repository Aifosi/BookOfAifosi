package bot.wheel

import bot.{Lurch, LurchLogger}
import bot.DiscordLogger
import bot.chaster.ChasterClient
import bot.chaster.model.{Lock, Segment}
import bot.commands.{Task as TaskCommand, TaskCompleter}
import bot.db.{RegisteredUserRepository, given}
import bot.db.Filters.*
import bot.db.PendingTaskRepository
import bot.model.{ChasterID, Message, RegisteredUser, given}
import bot.syntax.io.*
import bot.tasks.TextWheelCommand

import cats.data.{EitherT, OptionT}
import cats.effect.IO
import cats.syntax.functor.*
import doobie.implicits.*
import doobie.util.transactor.Transactor
import org.typelevel.log4cats.Logger
import scala.util.matching.Regex

class Task(
  client: ChasterClient,
  registeredUserRepository: RegisteredUserRepository,
  pendingTaskRepository: PendingTaskRepository,
  mySqlTransactor: Transactor[IO],
)(using LurchLogger)
    extends TextWheelCommand(client, registeredUserRepository):
  override val pattern: Regex = "Task: (.+)".r

  override def run(user: RegisteredUser, lock: Lock, text: String)(using Logger[IO]): IO[Boolean] =
    lazy val keyholder: OptionT[IO, RegisteredUser] =
      for
        chasterKeyholder <- OptionT.fromOption(lock.keyholder)
        keyholder        <- registeredUserRepository.find(chasterKeyholder._id.equalChasterID, user.guildID.equalGuildID)
      yield keyholder

    def addReaction(message: Message, task: TaskCommand): IO[Unit] =
      val either = for
        keyholder <- keyholder.toRight(s"Unable to find keyholder for lock ${lock._id}.")
        _         <- EitherT.liftF(pendingTaskRepository.add(task.title, message.id, user, keyholder, None))
        _         <- EitherT.liftF(message.addReaction(TaskCompleter.pattern))
      yield ()
      either.foldF(
        error => Logger[IO].warn(error),
        IO.pure,
      )

    Task.handleTask(text, user, mySqlTransactor, addReaction).value.as(true)

  override val description: String = "Grabs a task with the given tag and issues it to the lockee"

object Task:
  def handleTask(
    task: String,
    user: RegisteredUser,
    mySqlTransactor: Transactor[IO],
    andAlso: (Message, TaskCommand) => IO[Unit] = (_, _) => IO.unit,
  )(using logger: Logger[IO], discordLogger: LurchLogger): OptionT[IO, String] =
    OptionT(fr"call GetTask(${user.discordID}, $task)".query[TaskCommand].option.transact(mySqlTransactor))
      .flatTapNone(logger.warn(s"Unable to get task for ${user.discordID}, $task"))
      .map(_.cleanedDescription)
      .flatMap { task =>
        discordLogger
          .logToTortureChamber(s"${user.mention} rolled task ${task.id} - ${task.title}")
          .semiflatMap(andAlso(_, task))
          .as(s"Rolled task ${task.id} - ${task.title}\n${task.description}")
          .semiflatTap(user.sendMessage)
      }
