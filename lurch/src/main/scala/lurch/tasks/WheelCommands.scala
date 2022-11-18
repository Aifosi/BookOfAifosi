package lurch.tasks

import bot.chaster.{Event, Segment, WheelTurnedPayload}
import bot.chaster.Client.{*, given}
import bot.db.Filters.*
import bot.db.RegisteredUserRepository
import bot.model.{ChasterID, RegisteredUser}
import bot.tasks.{RepeatedStreams, WheelCommand}
import bot.syntax.io.*
import bot.syntax.stream.*
import cats.data.{NonEmptyList, OptionT}
import cats.effect.IO
import cats.syntax.functor.*
import cats.syntax.option.*
import cats.syntax.traverse.*
import fs2.Stream
import io.circe.Json
import lurch.Lurch
import lurch.db.RecentLockHistoryRepository
import lurch.model.RecentLockHistory
import lurch.wheel.*
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger

import java.time.Instant
import scala.concurrent.duration.FiniteDuration

object WheelCommands extends RepeatedStreams:
  lazy val commands: NonEmptyList[WheelCommand] = NonEmptyList.of(
    Once, // Needs to be first
    DiceMultiplier,
    VerificationPictures,
    PilloryTime,
    DiceRolls,
    WheelRolls,
    VoteTarget,
    VoteAdd,
    VoteRemove,
    AddSegments,
    Task,
  )

  private def handleEvent(user: RegisteredUser, event: Event[Json], lockID: ChasterID)(using Logger[IO]): IO[Unit] =
    val segment: OptionT[IO, Segment] = OptionT.when[IO, Option[Event[WheelTurnedPayload]]](event.`type` == "wheel_of_fortune_turned")(event.as[WheelTurnedPayload])
      .subflatMap(identity)
      .map(_.payload.segment)

    commands.foldLeft(segment) {
      case (optionT, command) =>
        for
          segment <- OptionT(optionT.value.logErrorOption.map(_.flatten))
          updatedSegment <- OptionT(command.apply(user, lockID, segment).map((stop, segment) => Option.unless(stop)(segment)))
        yield updatedSegment
    }.value.void

  private def handleHistory(user: RegisteredUser, lockID: ChasterID, mostRecentEventTime: Option[Instant])(using Logger[IO]): Stream[IO, Instant] =
    for
      event <- user.lockHistory(lockID, mostRecentEventTime)
      _ <- handleEvent(user, event, lockID).streamed
    yield event.createdAt

  override lazy val delay: FiniteDuration = Lurch.config.checkFrequency

  private def getLockHistory(user: RegisteredUser)(using Logger[IO]): Stream[IO, RecentLockHistory] =
    for
      lock <- Stream.evalSeq(user.locks)
      maybeLockHistory <- RecentLockHistoryRepository.find(lock._id.equalLockID).streamed
      lockHistory <- maybeLockHistory.fold(RecentLockHistoryRepository.add(user.id, lock._id, Instant.now().some).streamed)(Stream.emit)
    yield lockHistory

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