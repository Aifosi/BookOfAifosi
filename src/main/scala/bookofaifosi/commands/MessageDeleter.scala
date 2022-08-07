package bookofaifosi.commands

import bookofaifosi.db.LockedChannelsRepository
import bookofaifosi.db.Filters.*
import bookofaifosi.model.event.MessageEvent
import cats.effect.IO
import org.typelevel.log4cats.Logger

import scala.util.matching.Regex

object MessageDeleter extends TextCommand with Hidden:
  override def pattern: Regex = Command.all

  override def apply(pattern: Regex, event: MessageEvent)(using Logger[IO]): IO[Boolean] =
    for
      guild <- event.guild
      channelIsLocked <- LockedChannelsRepository.find(guild.discordID.equalGuildID, event.channel.discordID.equalChannelID).map(_.nonEmpty)
      _ <- if channelIsLocked then event.message.delete else IO.unit
    yield channelIsLocked

  override val description: String = "Deleted messages from locked channels"
