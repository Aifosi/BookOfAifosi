package bookofaifosi.commands

import bookofaifosi.Bot
import bookofaifosi.chaster.Client.*
import bookofaifosi.chaster.{Event, WheelTurnedPayload}
import bookofaifosi.commands.PatternOption
import bookofaifosi.model.{TaskSubscription, User}
import bookofaifosi.db.{RegisteredUserRepository, TaskSubscriptionRepository, UserRepository, User as DBUser}
import bookofaifosi.db.Filters.*
import bookofaifosi.model.event.{AutoCompleteEvent, SlashAPI, SlashCommandEvent}
import bookofaifosi.syntax.stream.*
import cats.effect.{IO, Ref}
import cats.data.{EitherT, OptionT}
import cats.syntax.option.*
import doobie.syntax.connectionio.*
import fs2.{Pure, Stream}
import io.circe.Json

import java.time.Instant
import scala.concurrent.duration.*

object Subscribe extends SlashCommand with Options with AutoCompleteString with Streams with SlowResponse:
  override val defaultEnabled: Boolean = true
  override val fullCommand: String = "wearer subscribe tasks"
  override val options: List[PatternOption] = List(
    _.addOption[String]("lock", "The lock you want to get messages about.", autoComplete = true)
  )

  private def lockNames(user: User): IO[List[String]] =
    (for
      user <- Stream.evalOption(RegisteredUserRepository.find(user.discordID.equalID))
      lock <- Stream.evalSeq(user.locks)
    yield lock.title).compile.toList

  override val autoCompleteOptions: Map[String, AutoCompleteEvent => IO[List[String]]] = Map(
    "lock" -> ((event: AutoCompleteEvent) => lockNames(event.author))
  )

  override val stream: Stream[IO, Unit] =
    for
      subscription @ TaskSubscription(registeredUser, user, lockID, mostRecentEventTime) <- Stream.evalSeq(TaskSubscriptionRepository.list()).metered(60.seconds).repeat
      event <- registeredUser.lockHistory(lockID, mostRecentEventTime)
      _ <- Stream.eval(if mostRecentEventTime.forall(_.isBefore(event.createdAt)) then TaskSubscriptionRepository.update(registeredUser.id, lockID, event.createdAt.some) else IO.unit)
      wheelTurnedEvent <- Stream.whenS(event.`type` == "wheel_of_fortune_turned")(event.as[WheelTurnedPayload])
      taskEvent <- Stream.when(wheelTurnedEvent.payload.segment.`type` == "text")(wheelTurnedEvent)
      task = taskEvent.payload.segment.text
      _ <- Stream.eval(user.sendMessage(s"You just rolled a task: $task."))
    yield ()

  override val ephemeralResponses: Boolean = true

  override def slowResponse(pattern: SlashPattern, event: SlashCommandEvent, slashAPI: Ref[IO, SlashAPI]): IO[Unit] =
    val response = for
      user <- OptionT(RegisteredUserRepository.find(event.author.discordID.equalID)).filter(_.isWearer).toRight("You need to register as a wearer use this command, please use `/register wearer` to do so.")
      lockTitle = event.getOption[String]("lock")
      lock <- OptionT(user.locks.map(_.find(_.title == lockTitle))).toRight(s"Can't find lock with name $lockTitle")
      lockId = lock._id
      mostRecentEvent <- EitherT(user.lockHistory(lockId).take(1).compile.last.attempt).leftMap(_ => s"Invalid lock id $lockId")
      _ <- EitherT.liftF(TaskSubscriptionRepository.add(user.id, lockId, mostRecentEvent.map(_.createdAt)))
    yield ()
    for
      response <- response.value
      slashAPI <- slashAPI.get
      _ <- response.fold(
        error => slashAPI.replyEphemeral(error),
        _ => slashAPI.replyEphemeral("You will now receive messages when you roll a task.")
      )
    yield ()

  override val description: String = "Sends you a message when you spin the wheel"
