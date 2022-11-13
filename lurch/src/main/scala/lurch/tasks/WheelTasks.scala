package lurch.tasks

import bot.Bot
import bot.chaster.Client.{*, given}
import bot.chaster.{Event, WheelTurnedPayload, SegmentType}
import bot.db.Filters.*
import bot.db.{RegisteredUserRepository, given}
import bot.model.event.{SlashAPI, SlashCommandEvent}
import bot.model.{ChasterID, Message, RegisteredUser}
import bot.syntax.io.*
import bot.syntax.stream.*
import bot.tasks.RepeatedStreams
import cats.data.{EitherT, OptionT}
import cats.effect.{IO, Ref}
import cats.syntax.functor.*
import cats.syntax.option.*
import cats.syntax.traverse.*
import doobie.implicits.*
import fs2.Stream
import io.circe.Json
import lurch.Lurch
import lurch.commands.{Task, TaskCompleter}
import lurch.db.{PendingTaskRepository, RecentLockHistoryRepository}
import lurch.model.RecentLockHistory
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger

import java.time.Instant
import scala.concurrent.duration.FiniteDuration

object WheelTasks extends RepeatedStreams:
  private val taskRegex = "Task: (.+)".r

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

  private def getLockHistory(user: RegisteredUser)(using Logger[IO]): Stream[IO, RecentLockHistory] =
    for
      lock <- Stream.evalSeq(user.locks)
      maybeLockHistory <- RecentLockHistoryRepository.find(lock._id.equalLockID).streamed
      lockHistory <- maybeLockHistory.fold(RecentLockHistoryRepository.add(user.id, lock._id, Instant.now().some).streamed)(Stream.emit)
    yield lockHistory

  private def handleEvent(user: RegisteredUser, event: Event[Json])(using Logger[IO]): IO[Unit] =
    OptionT.when[IO, Option[Event[WheelTurnedPayload]]](event.`type` == "wheel_of_fortune_turned")(event.as[WheelTurnedPayload])
      .subflatMap(identity)
      .collect {
        case event if event.payload.segment.`type` == SegmentType.Text => event.payload.segment.text
      }
      .semiflatTap(text => Logger[IO].debug(s"$user rolled $text"))
      .flatMap {
        case taskRegex(task) =>
          def addReaction(message: Message, task: Task): IO[Unit] =
            for
              registeredKeyholders <- user.registeredKeyholders
              _ <- PendingTaskRepository.add(task.title, message.id, user, registeredKeyholders, None)
              _ <- message.addReaction(TaskCompleter.pattern)
            yield ()

          handleTask(task, user, addReaction).void
        case text =>
          Lurch.channels.spinlog.sendMessage(s"${user.mention} rolled $text").void
      }
      .value
      .void

  private def handleHistory(user: RegisteredUser, lockID: ChasterID, mostRecentEventTime: Option[Instant])(using Logger[IO]): Stream[IO, Instant] =
    for
      event <- user.lockHistory(lockID, mostRecentEventTime)
      _ <- handleEvent(user, event).streamed
    yield event.createdAt

  override lazy val delay: FiniteDuration = Lurch.config.checkFrequency

  def handleUser(user: RegisteredUser)(using Logger[IO]): Stream[IO, Unit] =
    for
      RecentLockHistory(_, lockID, mostRecentEventTimeDB) <- getLockHistory(user)
      mostRecentEventTime <- handleHistory(user, lockID, mostRecentEventTimeDB).compile.toList.map(_.maxOption).streamed
      _ <- mostRecentEventTime.fold(Stream.unit) {
        case mostRecentEventTime if mostRecentEventTimeDB.forall(_.isBefore(mostRecentEventTime)) =>
          RecentLockHistoryRepository.update(user.id, lockID, mostRecentEventTime.some).streamed
        case _ => Stream.unit
      }
    yield ()

  override lazy val repeatedStream: Stream[IO, Unit] =
    for
      given Logger[IO] <- Slf4jLogger.create[IO].streamed
      user <- Stream.evalSeq(RegisteredUserRepository.list().map(_.filter(user => user.isLocked && user.keyholderIDs.nonEmpty)))
      _ <- handleUser(user).compile.drain.logErrorOption.streamed
    yield ()
