package bot.commands

import bot.commands.{SlashCommand, SlashPattern}
import bot.model.event.SlashCommandEvent
import cats.effect.IO
import bot.db.LockedChannelsRepository
import org.typelevel.log4cats.Logger

class LockChannel(
  lockedChannelsRepository: LockedChannelsRepository,
) extends SlashCommand:
  override val defaultEnabled: Boolean = false
  override val fullCommand: String = "lock channel"

  override def apply(pattern: SlashPattern, event: SlashCommandEvent)(using Logger[IO]): IO[Boolean] =
    for
      guild <- event.guild
      _ <- lockedChannelsRepository.add(guild.discordID, event.channel.discordID)
      _ <- event.replyEphemeral("New messages in this channel will be automatically deleted.")
    yield true

  override val description: String = "Deletes all new messages in this channel"
