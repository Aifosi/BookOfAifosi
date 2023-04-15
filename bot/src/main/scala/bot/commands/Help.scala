package bot.commands

import bot.model.Discord
import bot.model.event.SlashCommandEvent
import bot.tasks.*
import cats.data.NonEmptyList
import cats.syntax.foldable.*
import cats.effect.IO
import org.typelevel.log4cats.Logger

import scala.util.matching.Regex

class Help(commands: List[AnyCommand], tasks: NonEmptyList[Streams]) extends SlashCommand:
  override val isUserCommand: Boolean = true
  override val fullCommand: String = "help"
  override val description: String = "Shows help for existing commands"

  override def apply(pattern: SlashPattern, event: SlashCommandEvent)(using Logger[IO]): IO[Boolean] =
    val commandWithDescriptions = commands.filter {
      case _: Hidden => false
      case command: SlashCommand => command.isUserCommand
      case _ => true
    }.map(command => s"**${command.pattern}** - ${command.description}")

    val wheelCommandsWithDescriptions = tasks.collect {
      case wheelcommands: WheelCommands =>
        "Wheel commands:" +: wheelcommands.commands.map {
          case command: WheelCommand[?] => s"  **${command.pattern}** - ${command.description}"
        }.toList
    }.flatten


    event.replyEphemeral((commandWithDescriptions ++ wheelCommandsWithDescriptions).mkString("\n")).as(true)


