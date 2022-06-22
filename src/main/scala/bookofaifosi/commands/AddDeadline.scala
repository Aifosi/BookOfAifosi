package bookofaifosi.commands
import bookofaifosi.commands.PatternOption
import bookofaifosi.db.{LockTaskDeadlineRepository, PendingTaskRepository, RegisteredUserRepository, TaskSubscriptionRepository}
import bookofaifosi.model.{LockTaskDeadline, PendingTask, TaskSubscription, User}
import bookofaifosi.model.event.*
import cats.effect.{IO, Ref}
import fs2.Stream
import bookofaifosi.syntax.stream.*
import bookofaifosi.syntax.io.*
import bookofaifosi.chaster.Client.*
import bookofaifosi.chaster.WheelTurnedPayload
import bookofaifosi.db.Filters.*
import cats.data.{EitherT, OptionT}
import cats.syntax.applicative.*
import cats.syntax.option.*

import java.time.Instant
import scala.concurrent.duration.*

object AddDeadline extends SlashCommand with Options with AutoCompleteString with Streams with SlowResponse:
  override val defaultEnabled: Boolean = false
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

  private def addPendingTasks(delay: FiniteDuration): Stream[IO, Unit] = for
    LockTaskDeadline(lockID, keyholder, user, deadline, mostRecentEventTime) <- Stream.evalSeq(LockTaskDeadlineRepository.list()).metered(delay).repeat
    event <- user.lockHistory(lockID, mostRecentEventTime)
    _ <- Stream.whenF(mostRecentEventTime.forall(_.isBefore(event.createdAt)))(LockTaskDeadlineRepository.update(lockID, keyholder.id, deadline, event.createdAt.some))
    wheelTurnedEvent <- Stream.whenS(event.`type` == "wheel_of_fortune_turned")(event.as[WheelTurnedPayload])
    taskEvent <- Stream.when(wheelTurnedEvent.payload.segment.`type` == "text")(wheelTurnedEvent)
    task = taskEvent.payload.segment.text
    _ <- user.sendMessage(s"You have $deadline to finish the task \"$task\"").streamed
    _ <- PendingTaskRepository.add(task, user.id, keyholder.id, event.createdAt.plusSeconds(deadline.toSeconds)).streamed
  yield ()

  private def notifyDeadlineFailed(delay: FiniteDuration): Stream[IO, Unit] = for
    PendingTask(id, task, user, keyholder, deadline) <- Stream.evalSeq(PendingTaskRepository.list()).metered(delay).repeat
    _ <- Stream.filter(deadline.isBefore(Instant.now()))
    _ <- keyholder.sendMessage(s"${user.mention} failed task $task").streamed
    _ <- user.sendMessage(s"You failed task $task").streamed
    _ <- PendingTaskRepository.remove(id).streamed
  yield ()

  override def stream(delay: FiniteDuration): Stream[IO, Unit] = addPendingTasks(delay).concurrently(notifyDeadlineFailed(delay))

  override val ephemeralResponses: Boolean = true
  //TODO Adding new deadline replaces old one
  override def slowResponse(pattern: SlashPattern, event: SlashCommandEvent, slashAPI: Ref[IO, SlashAPI]): IO[Unit] =
    val response = for
      keyHolder <- OptionT(RegisteredUserRepository.find(event.author.discordID.equalID)).filter(_.isKeyholder)
        .toRight(s"You need to register as a keyholder use this command, please use `/${RegisterKeyholder.fullCommand}` to do so.")
      lockTitle = event.getOption[String]("lock")
      lock <- OptionT(keyHolder.keyholderLocks.find(_.title == lockTitle).compile.last)
        .toRight(s"Can't find lock with name $lockTitle")
      user <- OptionT(RegisteredUserRepository.find(lock.user.username.equalChasterName))
        .toRight(s"Your lockee needs to register as a wearer use this command, it can be done using `/${RegisterWearer.fullCommand}`.")
      lockId = lock._id
      duration = event.getOption[Long]("deadline")
      unitName = event.getOption[String]("unit")
      unit <- OptionT.fromOption(AutoComplete.timeUnits.get(unitName))
        .toRight(s"Invalid unit \"$unitName\"")
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
