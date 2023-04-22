package bot.commands

import bot.commands.{SlashCommand, SlashPattern}
import bot.db.Filters.*
import bot.db.PilloryBitchesRepository
import bot.model.event.SlashCommandEvent

import cats.effect.IO
import org.typelevel.log4cats.Logger

class DisablePilloryBitches(
  pilloryBitchesRepository: PilloryBitchesRepository,
) extends SlashCommand:
  override val isUserCommand: Boolean = false
  override val fullCommand: String    = "pillory bitches disable"

  override def apply(pattern: SlashPattern, event: SlashCommandEvent)(using Logger[IO]): IO[Boolean] =
    for
      guild <- event.guild
      _     <- pilloryBitchesRepository.remove(guild.discordID.equalGuildID)
      _     <- event.replyEphemeral(s"Pillory bitches disabled.")
    yield true

  override val description: String = "Disables pillory bitches on this server."
