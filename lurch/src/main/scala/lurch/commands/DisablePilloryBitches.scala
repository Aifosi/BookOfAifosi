package lurch.commands

import bot.commands.{SlashCommand, SlashPattern}
import bot.db.Filters.*
import bot.model.event.SlashCommandEvent
import cats.effect.IO
import lurch.db.PilloryBitchesRepository
import org.typelevel.log4cats.Logger

object DisablePilloryBitches extends SlashCommand:
  override val defaultEnabled: Boolean = false
  override val fullCommand: String = "pillory bitches disable"

  override def apply(pattern: SlashPattern, event: SlashCommandEvent)(using Logger[IO]): IO[Boolean] =
    for
      guild <- event.guild
      _ <- PilloryBitchesRepository.remove(guild.discordID.equalGuildID)
      _ <- event.replyEphemeral(s"Pillory bitches disabled.")
    yield true

  override val description: String = "Disables pillory bitches on this server."
