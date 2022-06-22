package bookofaifosi.commands
import bookofaifosi.commands.PatternOption
import bookofaifosi.db.{LockTaskDeadlineRepository, PendingTaskRepository, RegisteredUserRepository, TaskSubscriptionRepository}
import bookofaifosi.model.{LockTaskDeadline, PendingTask, TaskSubscription, User}
import bookofaifosi.model.event.*
import cats.effect.{IO, Ref}
import fs2.Stream
import bookofaifosi.syntax.stream.*
import bookofaifosi.chaster.Client.*
import bookofaifosi.chaster.WheelTurnedPayload
import bookofaifosi.db.Filters.*
import cats.data.{EitherT, OptionT}
import cats.syntax.applicative.*
import cats.syntax.option.*

import java.time.Instant
import scala.concurrent.duration.*

object AddDeadline extends SlashCommand with Options with AutoCompleteString with Streams with SlowResponse:
  override val defaultEnabled: Boolean = true
  override val fullCommand: String = "keyholder add deadline"
  override val description: String = "Lets you add a deadline to a wearer's lock tasks."

  override val options: List[PatternOption] = List(
    Options.lockName,
    Options.duration("deadline"),
    Options.timeUnit,
  )

  private def lockNames(user: User): IO[List[String]] =
    (for
      user <- Stream.evalOption(RegisteredUserRepository.find(user.discordID.equalID))
      lock <- user.keyholderLocks
    yield lock.title).compile.toList

  override val autoCompleteOptions: Map[String, AutoCompleteEvent => IO[List[String]]] = Map(
    "lock" -> ((event: AutoCompleteEvent) => lockNames(event.author)),
    AutoComplete.timeUnit,
  )

  private val addPendingTasks: Stream[IO, Unit] = for
    LockTaskDeadline(lockID, keyholder, user, deadline, mostRecentEventTime) <- Stream.evalSeq(LockTaskDeadlineRepository.list()).metered(60.seconds).repeat
    event <- user.lockHistory(lockID, mostRecentEventTime)
    _ <- Stream.eval(if mostRecentEventTime.forall(_.isBefore(event.createdAt)) then LockTaskDeadlineRepository.update(lockID, keyholder.id, deadline, event.createdAt.some) else IO.unit)
    wheelTurnedEvent <- Stream.whenS(event.`type` == "wheel_of_fortune_turned")(event.as[WheelTurnedPayload])
    taskEvent <- Stream.when(wheelTurnedEvent.payload.segment.`type` == "text")(wheelTurnedEvent)
    task = taskEvent.payload.segment.text
    _ <- Stream.eval(PendingTaskRepository.add(task, user.id, keyholder.id, event.createdAt.plusSeconds(deadline.toSeconds)))
  yield ()

  private val notifyDeadlineFailed: Stream[IO, Unit] = for
    PendingTask(_, task, user, keyholder, deadline) <- Stream.evalSeq(PendingTaskRepository.list()).metered(60.seconds).repeat
    _ <- Stream.eval {
      if deadline.isAfter(Instant.now()) then
        keyholder.sendMessage(s"${user.mention} failed task $task") *> user.sendMessage(s"You failed task $task")
      else
        IO.unit
    }
  yield ()

  override def stream: Stream[IO, Unit] = addPendingTasks.concurrently(notifyDeadlineFailed)

  override val ephemeralResponses: Boolean = true
  override def slowResponse(pattern: SlashPattern, event: SlashCommandEvent, slashAPI: Ref[IO, SlashAPI]): IO[Unit] =
    val response = for
      keyHolder <- OptionT(RegisteredUserRepository.find(event.author.discordID.equalID)).filter(_.isKeyholder).toRight(s"You need to register as a keyholder use this command, please use `/${RegisterKeyholder.fullCommand}` to do so.")
      lockTitle = event.getOption[String]("lock")
      lock <- OptionT(keyHolder.keyholderLocks.find(_.title == lockTitle).compile.last).toRight(s"Can't find lock with name $lockTitle")
      user <- OptionT(RegisteredUserRepository.find(lock.user.username.equalChasterName)).toRight(s"Your lockee needs to register as a wearer use this command, it can be done using `/${RegisterWearer.fullCommand}`.")
      lockId = lock._id
      duration = event.getOption[Long]("deadline")
      unitName = event.getOption[String]("unit")
      unit <- OptionT.fromOption(AutoComplete.timeUnits.get(unitName)).toRight(s"Invalid unit \"$unitName\"")
      deadLine = FiniteDuration(duration, unit)
      mostRecentEvent <- EitherT(user.lockHistory(lockId).take(1).compile.last.attempt).leftMap(_ => s"Invalid lock id $lockId")
      _ <- EitherT.liftF(LockTaskDeadlineRepository.add(lockId, keyHolder.id, user.id, deadLine, mostRecentEvent.map(_.createdAt)))
    yield "You will now receive messages when tasks in are not marked as complete before the deadline."
    for
      response <- response.value
      slashAPI <- slashAPI.get
      _ <- response.fold(
        error => slashAPI.replyEphemeral(error),
        response => slashAPI.replyEphemeral(response)
      )
    yield ()
