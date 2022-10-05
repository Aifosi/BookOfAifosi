package bookofaifosi.tasks

import bookofaifosi.Bot
import fs2.Stream
import cats.effect.{IO, Ref}
import org.typelevel.log4cats.Logger
import bookofaifosi.chaster.{Event, WheelTurnedPayload}
import bookofaifosi.chaster.Client.*
import bookofaifosi.chaster.Client.given
import bookofaifosi.commands.{Register, SlashPattern, Task}
import bookofaifosi.db.{RecentLockHistoryRepository, RegisteredUserRepository}
import bookofaifosi.db.given
import bookofaifosi.model.event.{SlashAPI, SlashCommandEvent}
import bookofaifosi.syntax.stream.*
import bookofaifosi.syntax.io.*
import bookofaifosi.model.{ChasterID, RecentLockHistory, RegisteredUser}
import cats.data.{EitherT, OptionT}
import cats.syntax.option.*
import cats.syntax.traverse.*
import bookofaifosi.db.Filters.*
import doobie.implicits.*
import io.circe.Json
import org.typelevel.log4cats.slf4j.Slf4jLogger

import java.time.Instant
import scala.concurrent.duration.FiniteDuration

object WheelTasks extends RepeatedStreams:
  private val taskRegex = "Task: (.+)".r

  private def sendMessageToTortureChamber(message: String)(using Logger[IO]): IO[Unit] =
    for
      tortureChamber <- Bot.config.tortureChamberChannel
      _ <- tortureChamber.fold(Logger[IO].debug("Torture chamber channel not configured."))(_.sendMessage(message))
    yield ()

  def handleTask(task: String, user: RegisteredUser)(using Logger[IO]): OptionT[IO, String] =
    OptionT(
      fr"call GetTask(${user.discordID}, $task)".query[Task].option.transact(Bot.mysqlTransactor).flatMap {
        _.fold(Logger[IO].warn(s"Unable to get task for ${user.discordID}, $task").as(None)) { task =>
          sendMessageToTortureChamber(s"${user.mention} rolled task ${task.id} - ${task.tittle}")
            .as(s"Rolled task ${task.id} - ${task.tittle}\n${task.description}".some)
        }
      }
    )

  private def getLockHistory(user: RegisteredUser)(using Logger[IO]): Stream[IO, RecentLockHistory] =
    for
      lock <- Stream.evalSeq(user.locks)
      maybeLockHistory <- RecentLockHistoryRepository.find(lock._id.equalLockID).streamed
      lockHistory <- maybeLockHistory.fold(RecentLockHistoryRepository.add(user.id, lock._id, Instant.now().some).streamed)(Stream.emit)
    yield lockHistory

  private def handleEvent(user: RegisteredUser, event: Event[Json])(using Logger[IO]): IO[Unit] =
    Option.when(event.`type` == "wheel_of_fortune_turned")(event.as[WheelTurnedPayload])
      .flatten
      .collect {
        case event if event.payload.segment.`type` == "text" => event.payload.segment.text
      }
      .fold(IO.unit){
        case taskRegex(task) =>
          for
            _ <- Logger[IO].debug(s"$user rolled $task")
            _ <- handleTask(task, user).foldF(IO.unit)(user.sendMessage)
          yield ()

        case text =>
          Logger[IO].debug(s"$user rolled $text") *> sendMessageToTortureChamber(s"${user.mention} rolled $text")
      }


  private def handleHistory(user: RegisteredUser, lockID: ChasterID, mostRecentEventTime: Option[Instant])(using Logger[IO]): Stream[IO, Instant] =
    for
      event <- user.lockHistory(lockID, mostRecentEventTime)
      _ <- handleEvent(user, event).streamed
    yield event.createdAt

  override lazy val delay: FiniteDuration = Bot.config.checkFrequency

  override lazy val repeatedStream: Stream[IO, Unit] =
    for
      given Logger[IO] <- Slf4jLogger.create[IO].streamed
      user <- Stream.evalSeq(RegisteredUserRepository.list().map(_.filter(user => user.isLocked && user.keyholderIDs.nonEmpty)))
      RecentLockHistory(_, lockID, mostRecentEventTimeDB) <- getLockHistory(user)
      mostRecentEventTime <- handleHistory(user, lockID, mostRecentEventTimeDB).compile.toList.map(_.maxOption).streamed
      _ <- mostRecentEventTime.fold(Stream.unit) {
        case mostRecentEventTime if mostRecentEventTimeDB.forall(_.isBefore(mostRecentEventTime)) =>
          RecentLockHistoryRepository.update(user.id, lockID, mostRecentEventTime.some).streamed
        case _ => Stream.unit
      }
    yield ()
