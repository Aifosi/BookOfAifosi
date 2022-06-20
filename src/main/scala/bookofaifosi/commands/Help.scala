package bookofaifosi.commands

import cats.data.NonEmptyList
import cats.effect.IO
import bookofaifosi.Bot
import bookofaifosi.model.event.{MessageEvent, SlashCommandEvent}

import scala.util.matching.Regex

object Help extends SlashCommand:
  override val defaultEnabled: Boolean = true

  override val fullCommand: String = "help"

  override def apply(pattern: SlashPattern, event: SlashCommandEvent): IO[Boolean] =
    val message = Bot.allCommands.filter(!_.isInstanceOf[Hidden]).map { command =>
      s"`${command.pattern}` - ${command.description}"
    }.mkString("\n")

    event.replyEphemeral(message).as(true)

  override val description: String = "Show all existing commands and their descriptions"
