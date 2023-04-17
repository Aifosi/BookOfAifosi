package bot

import bot.commands
import bot.commands.*
import bot.db.{RegisteredUserRepository, UserTokenRepository}
import bot.model.{Discord, DiscordID, RegisteredUser}
import bot.syntax.action.*
import bot.syntax.stream.*
import bot.tasks.Streams
import cats.data.{EitherT, NonEmptyList}
import cats.effect.{Deferred, IO}
import cats.syntax.traverse.*
import cats.syntax.foldable.*
import fs2.Stream
import org.typelevel.log4cats.Logger
import scala.util.chaining.*

case class Commander[Log <: DiscordLogger](
  logger: Log,
  commands: List[AnyCommand],
  tasks: NonEmptyList[Streams],
  onDiscordAcquired: Discord => IO[Unit],
  unregisterHooks: Set[RegisteredUser => EitherT[IO, String, Unit]] = Set.empty,
)(using Logger[IO]):
  lazy val textCommands: List[TextCommand] = commands.collect {
    case command: TextCommand => command
  }
  lazy val reactionCommands: List[ReactionCommand] = commands.collect {
    case command: ReactionCommand => command
  }
  lazy val slashCommands: List[SlashCommand] = commands.collect {
    case command: SlashCommand => command
  }
  lazy val autoCompletableCommands: List[AutoCompletable] = commands.collect {
    case command: AutoCompletable => command
  }

  lazy val combinedTasks: Stream[IO, Unit] =
    tasks.map(_.stream.logErrorAndContinue()).reduceLeft(_.concurrently(_))

  def registerSlashCommands(discord: Discord): IO[Unit] =
    lazy val slashCommandData = SlashPattern.buildCommands(slashCommands.map(_.pattern))
    discord.guilds.traverse_ { guild =>
      for
        commandsAdded <- guild.addCommands(slashCommandData)
        commands <- guild.commands
        _ <- commands.collect {
          case jdaCommand if !commandsAdded.contains(DiscordID(jdaCommand.getIdLong)) => jdaCommand.delete.toIO
        }.sequence_
      yield ()
    } *> Logger[IO].info("All Slash commands registered.")

  def withDefaults(
    registeredUserRepository: RegisteredUserRepository,
    userTokenRepository: UserTokenRepository,
    registration: Registration,
  ): Commander[Log] =
    val alwaysEnabled = List(
      new Register(registration),
      new Unregister(registration),
      new Nuke(registeredUserRepository, userTokenRepository),
      Compress,
      new WheelCommandsList(tasks),
      WheelCommandsHelp,
    )
    val allCommands = (alwaysEnabled ++ commands)
      .pipe(commands => commands :+ new Help(commands))
    copy(commands = allCommands)
