package bookofaifosi.commands

import bookofaifosi.db.PilloryBitchesRepository
import bookofaifosi.model.event.SlashCommandEvent
import cats.effect.IO
import org.typelevel.log4cats.Logger

object EnablePilloryBitches extends SlashCommand:
  override val defaultEnabled: Boolean = false
  override val fullCommand: String = "pillory bitches enable"

  override def apply(pattern: SlashPattern, event: SlashCommandEvent)(using Logger[IO]): IO[Boolean] =
    for
      guild <- event.guild
      _ <- PilloryBitchesRepository.addOrUpdate(guild.discordID, event.channel.discordID)
      _ <- event.replyEphemeral(s"Pillory bitches enabled on this channel. Winner messages will be sent on this channel.")
    yield true

  override val description: String = "Enables pillory bitches for this server on the channel used."
