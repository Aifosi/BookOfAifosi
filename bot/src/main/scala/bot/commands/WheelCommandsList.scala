package bot.commands

import bot.model.Discord
import bot.model.event.SlashCommandEvent
import bot.tasks.*

import cats.data.NonEmptyList
import cats.effect.IO
import cats.syntax.foldable.*
import org.typelevel.log4cats.Logger

class WheelCommandsList(tasks: NonEmptyList[Streams]) extends SlashCommand:
  override val isUserCommand: Boolean = true
  override val fullCommand: String    = WheelCommandsList.fullCommand
  override val description: String    = "Shows a list of existing wheel commands"

  override def apply(pattern: SlashPattern, event: SlashCommandEvent)(using Logger[IO]): IO[Boolean] =
    val wheelCommandsWithDescriptions = tasks.collect { case wheelcommands: WheelCommands =>
      s"Wheel commands (see /${WheelCommandsHelp.fullCommand} for more detailed information on how to use them):" +: wheelcommands.commands.map {
        case command: WheelCommand[?] => s"**${command.pattern}** - ${command.description}"
      }.toList
    }.flatten

    event.replyEphemeral(wheelCommandsWithDescriptions.mkString("\n")).as(true)

object WheelCommandsList:
  val fullCommand = "wheel commands list"
