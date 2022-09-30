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
import bookofaifosi.db.Filters.*
import doobie.implicits.*
import io.circe.Json
import org.typelevel.log4cats.slf4j.Slf4jLogger

import java.time.Instant
import scala.concurrent.duration.FiniteDuration

object WheelTasks extends RepeatedStreams:
  private val taskRegex = "Task: (.+)".r

  private def sendMessageToTortureChamber(message: String): IO[Unit] =
    for
      tortureChamber <- Bot.config.tortureChamberChannel
      _ <- tortureChamber.fold(IO.unit)(_.sendMessage(message))
    yield ()

  private def handleTask(task: String, user: RegisteredUser): IO[Unit] =
    for
      maybeTask <- fr"call GetTask(${user.discordID}, $task)".query[Task].option.transact(Bot.mysqlTransactor)
      task <- IO.fromOption(maybeTask)(new Exception("Failed to get task."))
      _ <- user.sendMessage(s"Rolled task ${task.id} - ${task.tittle}")
      _ <- user.sendMessage(task.description)
      _ <- sendMessageToTortureChamber(s"${user.mention} rolled task ${task.id} - ${task.tittle}")
    yield ()


  private def getLockHistory(user: RegisteredUser)(using Logger[IO]) =
    OptionT(RecentLockHistoryRepository.find(user.id.equalUserID)).getOrElseF {
      for
        locks <- user.locks
        lock <- IO.fromOption(locks.headOption)(new Exception("Failed to get locks"))
        lockID = lock._id
        event <- (user.lockHistory(lockID, None): Stream[IO, Event[Json]]).compile.last
        lockHistory <- RecentLockHistoryRepository.add(user.id, lockID, event.map(_.createdAt))
      yield lockHistory
    }

  private def handleEvent(user: RegisteredUser, event: Event[Json])(using Logger[IO]): IO[Unit] =
    Option.when(event.`type` == "wheel_of_fortune_turned")(event.as[WheelTurnedPayload])
      .flatten
      .collect {
        case event if event.payload.segment.`type` == "text" => event.payload.segment.text
      }
      .fold(IO.unit){
        case taskRegex(task) => handleTask(task, user)
        case text => sendMessageToTortureChamber(s"${user.mention} rolled $text")
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
      RecentLockHistory(_, lockID, mostRecentEventTimeDB) <- Stream.eval(getLockHistory(user))
      mostRecentEventTime <- handleHistory(user, lockID, mostRecentEventTimeDB).compile.last.streamed
      _ <- mostRecentEventTime.fold(Stream.unit) { mostRecentEventTime =>
        if mostRecentEventTimeDB.forall(_.isBefore(mostRecentEventTime)) then RecentLockHistoryRepository.update(user.id, lockID, mostRecentEventTime.some).streamed else Stream.unit
      }
    yield ()
