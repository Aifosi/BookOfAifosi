package bookofaifosi.commands

import bookofaifosi.db.PilloryBitchesRepository
import bookofaifosi.db.Filters.*
import bookofaifosi.model.event.SlashCommandEvent
import cats.effect.IO
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
