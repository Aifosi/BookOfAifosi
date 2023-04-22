package bot.commands

import bot.commands.{SlashCommand, SlashPattern}
import bot.db.Filters.*
import bot.db.LockedChannelsRepository
import bot.model.event.SlashCommandEvent

import cats.effect.IO
import org.typelevel.log4cats.Logger

class UnlockChannel(
  lockedChannelsRepository: LockedChannelsRepository,
) extends SlashCommand:
  override val isUserCommand: Boolean = false
  override val fullCommand: String    = "unlock channel"

  override def apply(pattern: SlashPattern, event: SlashCommandEvent)(using Logger[IO]): IO[Boolean] =
    for
      guild                <- event.guild
      lockedChannelRemoved <- lockedChannelsRepository
                                .remove(guild.discordID.equalGuildID, event.channel.discordID.equalChannelID)
                                .map(_ == 1)
      message               = if lockedChannelRemoved then "No longer deleting new messages on this channel."
                              else "This channel was not locked!"
      _                    <- event.replyEphemeral(message)
    yield true

  override val description: String = "Stops deleting messages in this channel"
