package lurch.commands

import bot.commands.{SlashCommand, SlashPattern}
import bot.model.event.SlashCommandEvent
import cats.effect.IO
import lurch.db.PilloryBitchesRepository
import org.typelevel.log4cats.Logger

class EnablePilloryBitches(
  pilloryBitchesRepository: PilloryBitchesRepository,
) extends SlashCommand:
  override val defaultEnabled: Boolean = false
  override val fullCommand: String = "pillory bitches enable"

  override def apply(pattern: SlashPattern, event: SlashCommandEvent)(using Logger[IO]): IO[Boolean] =
    for
      guild <- event.guild
      _ <- pilloryBitchesRepository.addOrUpdate(guild.discordID, event.channel.discordID)
      _ <- event.replyEphemeral(s"Pillory bitches enabled on this channel. Winner messages will be sent on this channel.")
    yield true

  override val description: String = "Enables pillory bitches for this server on the channel used."
