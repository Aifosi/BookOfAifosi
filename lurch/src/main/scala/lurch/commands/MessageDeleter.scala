package lurch.commands

import bot.commands.{Command, Hidden, TextCommand}
import bot.db.Filters.*
import bot.model.event.MessageEvent
import cats.effect.IO
import lurch.db.LockedChannelsRepository
import org.typelevel.log4cats.Logger

import scala.util.matching.Regex

class MessageDeleter(
  lockedChannelsRepository: LockedChannelsRepository,
) extends TextCommand with Hidden:
  override def pattern: Regex = Command.all

  override def apply(pattern: Regex, event: MessageEvent)(using Logger[IO]): IO[Boolean] =
    for
      guild <- event.guild
      channelIsLocked <- lockedChannelsRepository.find(guild.discordID.equalGuildID, event.channel.discordID.equalChannelID).isDefined
      _ <- if channelIsLocked then event.message.delete else IO.unit
    yield channelIsLocked

  override val description: String = "Deleted messages from locked channels"
