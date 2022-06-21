package bookofaifosi.commands
import bookofaifosi.commands.Options.PatternOptions
import bookofaifosi.db.RegisteredUserRepository
import bookofaifosi.model.User
import bookofaifosi.model.event.*
import cats.effect.{IO, Ref}
import fs2.Stream
import bookofaifosi.syntax.stream.*
import bookofaifosi.chaster.Client.*
import bookofaifosi.db.Filters.*

object Deadline extends SlashCommand with Options with AutoCompleteString with Streams with SlowResponse:
  override val defaultEnabled: Boolean = true
  override val fullCommand: String = "deadline tasks"
  override val options: List[PatternOptions] = List(
    _.addOption[String]("lock", "Lock you want to add a deadline for the tasks", autoComplete = true),
    _.addOption[Int]("deadline", "Hours"),
  )

  private def lockNames(user: User): IO[List[String]] =
    (for
      user <- Stream.evalOption(RegisteredUserRepository.find(user.discordID.equalID))
      lock <- user.keyholderLocks
    yield lock.title).compile.toList

  override val autoCompleteOptions: Map[String, AutoCompleteEvent => IO[List[String]]] = Map(
    "lock" -> ((event: AutoCompleteEvent) => lockNames(event.author))
  )

  override def stream: Stream[IO, Unit] = Stream.empty

  override val ephemeralResponses: Boolean = true

  override def slowResponse(pattern: SlashPattern, event: SlashCommandEvent, slashAPI: Ref[IO, SlashAPI]): IO[Unit] = IO.unit
  override val description: String = "Lets you add a deadline to a wearer's lock"
