package bot.commands

import bot.model.Discord
import bot.model.event.SlashCommandEvent
import bot.tasks.*

import cats.data.NonEmptyList
import cats.effect.IO
import cats.syntax.foldable.*
import org.typelevel.log4cats.Logger
import scala.util.matching.Regex

class Help(commands: List[AnyCommand]) extends SlashCommand:
  override val isUserCommand: Boolean = true
  override val fullCommand: String    = "help"
  override val description: String    = "Shows help for existing commands"

  override def apply(pattern: SlashPattern, event: SlashCommandEvent)(using Logger[IO]): IO[Boolean] =
    val commandWithDescriptions = commands.filter {
      case _: Hidden             => false
      case command: SlashCommand => command.isUserCommand
      case _                     => true
    }.map(command => s"**${command.pattern}** - ${command.description}")
    event.replyEphemeral(commandWithDescriptions.mkString("\n")).as(true)
