package bookofaifosi.commands
import bookofaifosi.commands.PatternOption
import bookofaifosi.db.{RegisteredUserRepository, TaskSubscriptionRepository}
import bookofaifosi.model.User
import bookofaifosi.model.event.*
import cats.effect.{IO, Ref}
import fs2.Stream
import bookofaifosi.syntax.stream.*
import bookofaifosi.chaster.Client.*
import bookofaifosi.db.Filters.*
import cats.data.{EitherT, OptionT}
import cats.syntax.applicative.*

object Deadline extends SlashCommand with Options with AutoCompleteString with Streams with SlowResponse:
  override val defaultEnabled: Boolean = true
  override val fullCommand: String = "deadline tasks"
  override val description: String = "Lets you add a deadline to a wearer's lock"

  override val options: List[PatternOption] = List(
    _.addOption[String]("lock", "Lock you want to add a deadline for the tasks", autoComplete = true),
    _.addOption[Int]("deadline", "Hours"),
  )

  private def lockNames(user: User): IO[List[String]] =
    (for
      user <- Stream.evalOption(RegisteredUserRepository.find(user.discordID.equalID))
      lock <- user.keyholderLocks
    yield lock.title).compile.toList

  override val autoCompleteOptions: Map[String, AutoCompleteEvent => IO[List[String]]] = Map(
    "lock" -> ((event: AutoCompleteEvent) => lockNames(event.author)),
    "unit" -> (_ => Reminder.timeUnits.keys.toList.pure),
  )

  override def stream: Stream[IO, Unit] = Stream.empty

  override val ephemeralResponses: Boolean = true
  override def slowResponse(pattern: SlashPattern, event: SlashCommandEvent, slashAPI: Ref[IO, SlashAPI]): IO[Unit] =
    val response = for
      user <- OptionT(RegisteredUserRepository.find(event.author.discordID.equalID)).filter(_.isKeyholder).toRight("You need to register as a keyholder use this command, please use `/register keyholder` to do so.")
      lockTitle = event.getOption[String]("lock")
      lock <- OptionT(user.locks.map(_.find(_.title == lockTitle))).toRight(s"Can't find lock with name $lockTitle")
      lockId = lock._id
      mostRecentEvent <- EitherT(user.lockHistory(lockId).take(1).compile.last.attempt).leftMap(_ => s"Invalid lock id $lockId")
      _ <- EitherT.liftF(IO.println(s"Most recent event was at ${mostRecentEvent.map(_.createdAt)}"))
      //_ <- EitherT.liftF(TaskSubscriptionRepository.add(user.id, lockId, mostRecentEvent.map(_.createdAt)))
    yield ()
    for
      response <- response.value
      slashAPI <- slashAPI.get
      _ <- response.fold(
        error => slashAPI.replyEphemeral(error),
        _ => slashAPI.replyEphemeral("You will now receive messages when you roll a task.")
      )
    yield ()
