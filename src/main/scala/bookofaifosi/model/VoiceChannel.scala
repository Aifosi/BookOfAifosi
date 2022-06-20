package bookofaifosi.model

import cats.effect.IO
import bookofaifosi.syntax.action.*
import net.dv8tion.jda.api.Permission
import net.dv8tion.jda.api.entities.VoiceChannel as JDAVoiceChannel

import scala.jdk.CollectionConverters.*

class VoiceChannel(channel: JDAVoiceChannel):
  def members: List[Member] = channel.getMembers.asScala.toList.map(new Member(_))

  def toggleMuteAll: List[IO[Unit]] = members.map(_.toggleMute)
  def muteAll: List[IO[Unit]] = members.map(_.mute)
  def unmuteAll: List[IO[Unit]] = members.map(_.unmute)
