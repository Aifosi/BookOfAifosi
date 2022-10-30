package bot.commands

import cats.data.NonEmptyList
import cats.effect.IO
import bot.Bot
import bot.model.event.{MessageEvent, SlashCommandEvent}
import org.typelevel.log4cats.Logger

import scala.util.matching.Regex

object Help extends SlashCommand:
  override val defaultEnabled: Boolean = true

  override val fullCommand: String = "help"

  override def apply(pattern: SlashPattern, event: SlashCommandEvent)(using Logger[IO]): IO[Boolean] =
    val message = Bot.instance.allCommands.filter(!_.isInstanceOf[Hidden]).map {
      case command: SlashCommand => s"`${command.fullCommand}` - ${command.description}"
      case command => s"`${command.pattern}` - ${command.description}"
    }.mkString("\n")
    event.replyEphemeral(message).as(true)

  override val description: String = "Show all existing commands and their descriptions"
