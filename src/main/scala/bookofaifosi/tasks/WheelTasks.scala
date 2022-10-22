package bookofaifosi.tasks

import bookofaifosi.Bot
import fs2.Stream
import cats.effect.{IO, Ref}
import org.typelevel.log4cats.Logger
import bookofaifosi.chaster.{Event, WheelTurnedPayload}
import bookofaifosi.chaster.Client.*
import bookofaifosi.chaster.Client.given
import bookofaifosi.commands.{Task, TaskCompleter}
import bookofaifosi.db.{PendingTaskRepository, RecentLockHistoryRepository, RegisteredUserRepository, given}
import bookofaifosi.model.event.{SlashAPI, SlashCommandEvent}
import bookofaifosi.syntax.stream.*
import bookofaifosi.syntax.io.*
import bookofaifosi.model.{ChasterID, Message, RecentLockHistory, RegisteredUser}
import cats.data.{EitherT, OptionT}
import cats.syntax.option.*
import cats.syntax.traverse.*
import cats.syntax.functor.*
import bookofaifosi.db.Filters.*
import doobie.implicits.*
import io.circe.Json
import org.typelevel.log4cats.slf4j.Slf4jLogger

import java.time.Instant
import scala.concurrent.duration.FiniteDuration

object WheelTasks extends RepeatedStreams:
  private val taskRegex = "Task: (.+)".r

  def handleTask(task: String, user: RegisteredUser, andAlso: (Message, Task) => IO[Unit] = (_, _) => IO.unit)(using Logger[IO]): OptionT[IO, String] =
    OptionT(fr"call GetTask(${user.discordID}, $task)".query[Task].option.transact(Bot.mysqlTransactor))
      .flatTapNone(Logger[IO].warn(s"Unable to get task for ${user.discordID}, $task"))
      .map(_.cleanedDescription)
      .flatMap { task =>
        Bot.config.channels.tortureChamber.sendMessage(s"${user.mention} rolled task ${task.id} - ${task.title}")
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
        case event if event.payload.segment.`type` == "text" => event.payload.segment.text
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
          Bot.config.channels.spinlog.sendMessage(s"${user.mention} rolled $text").void
      }
      .value
      .void

  private def handleHistory(user: RegisteredUser, lockID: ChasterID, mostRecentEventTime: Option[Instant])(using Logger[IO]): Stream[IO, Instant] =
    for
      event <- user.lockHistory(lockID, mostRecentEventTime)
      _ <- handleEvent(user, event).streamed
    yield event.createdAt

  override lazy val delay: FiniteDuration = Bot.config.checkFrequency

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
