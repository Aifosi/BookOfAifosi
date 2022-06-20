package bookofaifosi.model

import java.net.URL

import cats.effect.IO
import bookofaifosi.syntax.action.*
import net.dv8tion.jda.api.entities.User as JDAUser
import compiletime.asMatchable
import scala.jdk.CollectionConverters.*

open class User(private[model] val user: JDAUser):
  lazy val discordID: Long = user.getIdLong
  lazy val mention: String = s"<@!$discordID>"
  lazy val name: String = user.getName
  lazy val tag: String = user.getAsTag
  def getNameIn(guild: Guild): String = guild.getMember(this).flatMap(_.nickname).getOrElse(name)
  lazy val avatarURL: URL = new URL(user.getAvatarUrl)
  lazy val isBot: Boolean = user.isBot

  lazy val privateChannel: IO[PrivateChannel] = user.openPrivateChannel.toIO.map(new PrivateChannel(_))

  def sendMessage(message: String): IO[Message] = privateChannel.flatMap(_.sendMessage(message))

  def voiceChannel: Option[VoiceChannel] = user.getJDA.getVoiceChannels.asScala.toList.collectFirst {
    case channel if channel.getMembers.asScala.exists(_.getUser.getIdLong == user.getIdLong) =>
      new VoiceChannel(channel)
  }

  def member: Option[Member] = voiceChannel.flatMap(_.members.find(_.id == discordID))

  def isSelfMuted: Option[Boolean] = member.map(_.isSelfMuted)

  override def toString: String = s"$tag($discordID)"

  def canEqual(other: Any): Boolean = other.isInstanceOf[User]

  override def equals(other: Any): Boolean = other.asMatchable match
    case that: User =>
      that.canEqual(this) &&
        discordID == that.discordID
    case _ => false

  override def hashCode(): Int =
    val state = Seq(discordID)
    state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
