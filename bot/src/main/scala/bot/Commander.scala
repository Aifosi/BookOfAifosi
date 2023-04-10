package bot

import bot.commands
import bot.commands.*
import bot.db.{RegisteredUserRepository, UserTokenRepository}
import bot.model.{Discord, RegisteredUser}
import bot.syntax.stream.*
import bot.tasks.Streams
import cats.data.{NonEmptyList, EitherT}
import cats.effect.{Deferred, IO}
import cats.syntax.traverse.*
import cats.syntax.foldable.*
import fs2.Stream
import org.typelevel.log4cats.Logger

case class Commander[Log <: DiscordLogger](
  logger: Log,
  allCommands: List[AnyCommand],
  tasks: NonEmptyList[Streams],
  onDiscordAcquired: Discord => IO[Unit],
  unregisterHooks: Set[RegisteredUser => EitherT[IO, String, Unit]] = Set.empty,
)(using Logger[IO]):
  lazy val textCommands: List[TextCommand] = allCommands.collect {
    case command: TextCommand => command
  }
  lazy val reactionCommands: List[ReactionCommand] = allCommands.collect {
    case command: ReactionCommand => command
  }
  lazy val slashCommands: List[SlashCommand] = allCommands.collect {
    case command: SlashCommand => command
  }
  lazy val autoCompletableCommands: List[AutoCompletable] = allCommands.collect {
    case command: AutoCompletable => command
  }

  lazy val combinedTasks: Stream[IO, Unit] =
    tasks.map(_.stream.logErrorAndContinue()).reduceLeft(_.concurrently(_))

  def registerSlashCommands(discord: Discord): IO[Unit] =
    val patterns = SlashPattern.buildCommands(slashCommands.map(_.pattern))
    discord.guilds.traverse_(_.addCommands(patterns)) *> Logger[IO].info("All Slash commands registered.")

  def withDefaults(
    registeredUserRepository: RegisteredUserRepository,
    userTokenRepository: UserTokenRepository,
    registration: Registration,
  ): Commander[Log] =
    val alwaysEnabled = List(
      new Register(registration),
      new Unregister(registration),
      new Nuke(registeredUserRepository, userTokenRepository),
    )
    copy(allCommands = alwaysEnabled ++ allCommands)
