package bookofaifosi.commands
import bookofaifosi.db.LockedChannelsRepository
import bookofaifosi.model.event.SlashCommandEvent
import cats.effect.IO
import org.typelevel.log4cats.Logger

object LockChannel extends SlashCommand:
  override val defaultEnabled: Boolean = false
  override val fullCommand: String = "lock channel"

  override def apply(pattern: SlashPattern, event: SlashCommandEvent)(using Logger[IO]): IO[Boolean] =
    for
      guild <- event.guild
      _ <- LockedChannelsRepository.add(guild.discordID, event.channel.discordID)
      _ <- event.replyEphemeral("New messages in this channel will be automatically deleted.")
    yield true

  override val description: String = "Deletes all new messages in this channel"
