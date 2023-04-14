package bot.tasks

import bot.{Bot, DiscordLogger}
import bot.chaster.{ChasterClient, Event, Lock, Segment, SegmentType, WheelTurnedPayload}
import bot.db.Filters.*
import bot.db.RegisteredUserRepository
import bot.model.{ChasterID, RegisteredUser}
import bot.tasks.{RepeatedStreams, WheelCommand}
import bot.syntax.io.*
import bot.syntax.stream.*
import cats.data.{NonEmptyList, OptionT}
import cats.effect.{IO, Ref}
import cats.syntax.functor.*
import cats.syntax.option.*
import cats.syntax.traverse.*
import fs2.Stream
import io.circe.Json
import bot.db.RecentLockHistoryRepository
import bot.model.RecentLockHistory
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger

import java.time.Instant
import scala.concurrent.duration.FiniteDuration

class WheelCommands(
  chasterClient: ChasterClient,
  registeredUserRepository: RegisteredUserRepository,
  recentLockHistoryRepository: RecentLockHistoryRepository,
  commands: NonEmptyList[WheelCommand],
  override val delay: FiniteDuration,
)(using discordLogger: DiscordLogger) extends RepeatedStreams:
  private def handleEvent(user: RegisteredUser, event: Event[Json], lock: Lock)(using Logger[IO]): IO[Unit] =
    val segment: OptionT[IO, Segment] = OptionT.when[IO, Option[Event[WheelTurnedPayload]]](event.`type` == "wheel_of_fortune_turned")(event.as[WheelTurnedPayload])
      .subflatMap(identity)
      .map(_.payload.segment)

    commands.foldLeft(segment) {
      case (optionT, command) =>
        for
          segment <- OptionT(optionT.value.logErrorOption.map(_.flatten))
          updatedSegment <- OptionT(command.apply(user, lock, segment).map((stop, segment) => Option.unless(stop)(segment)))
        yield updatedSegment
    }.collect {
      case segment @ Segment(text, SegmentType.Text, _) =>
        for
          _ <- discordLogger.logToSpinlog(s"${user.mention} rolled $text")
          _ <- Logger[IO].debug(s"$user rolled $text")
        yield segment
    }.value.void

  private def handleHistory(user: RegisteredUser, lock: Lock, mostRecentEventTime: Option[Instant])(using Logger[IO]): Stream[IO, Instant] =
    for
      event <- chasterClient.authenticatedEndpoints(user.token).lockHistory(lock._id, mostRecentEventTime)
      _ <- handleEvent(user, event, lock).streamed
    yield event.createdAt

  private def getLockHistory(user: RegisteredUser)(using Logger[IO]): Stream[IO, (Lock, RecentLockHistory)] =
    for
      lock <- Stream.evalSeq(chasterClient.authenticatedEndpoints(user.token).locks)
      maybeLockHistory <- recentLockHistoryRepository.find(lock._id.equalLockID).value.streamed
      lockHistory <- maybeLockHistory.fold(recentLockHistoryRepository.add(user.id, lock._id, Instant.now().some).streamed)(Stream.emit)
    yield (lock, lockHistory)

  def handleUser(user: RegisteredUser)(using Logger[IO]): Stream[IO, Unit] =
    for
      (lock, RecentLockHistory(_, lockID, mostRecentEventTimeDB)) <- getLockHistory(user)
      mostRecentEventTime <- handleHistory(user, lock, mostRecentEventTimeDB).compile.toList.map(_.maxOption).streamed
      _ <- mostRecentEventTime.fold(Stream.unit) {
        case mostRecentEventTime if mostRecentEventTimeDB.forall(_.isBefore(mostRecentEventTime)) =>
          recentLockHistoryRepository.update(user.id, lockID, mostRecentEventTime.some).streamed
        case _ => Stream.unit
      }
    yield ()

  override lazy val repeatedStream: Stream[IO, Unit] =
    for
      given Logger[IO] <- Slf4jLogger.create[IO].streamed
      registeredUsersOrLeft <- Stream.eval(registeredUserRepository.thoroughList())
      (left, registeredUsers) = registeredUsersOrLeft.partitionMap(identity)
      _ <- if left.isEmpty then Stream.unit else Stream.emits(left).flatMap(left => discordLogger.logWithoutSpam(s"Skipping wheel tasks for user that left server: $left"))
      user <- Stream.emits(registeredUsers.filter(user => user.isLocked && user.keyholderIDs.nonEmpty))
      _ <- handleUser(user).compile.drain.logErrorOption.streamed
    yield ()