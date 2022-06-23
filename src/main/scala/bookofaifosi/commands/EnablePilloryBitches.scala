package bookofaifosi.commands
import bookofaifosi.db.PilloryBitchesRepository
import bookofaifosi.model.event.SlashCommandEvent
import cats.effect.IO

object EnablePilloryBitches extends SlashCommand:
  override val defaultEnabled: Boolean = false
  override val fullCommand: String = "pillory bitches enable"

  override def apply(pattern: SlashPattern, event: SlashCommandEvent): IO[Boolean] =
    for
      _ <- PilloryBitchesRepository.addOrUpdate(event.guild.get.discordID, event.channel.discordID)
      _ <- event.replyEphemeral(s"Pillory bitches enabled on this channel. Winner messages will be sent on this channel.")
    yield true

  override val description: String = "Enables pillory bitches for this server on the channel used."
