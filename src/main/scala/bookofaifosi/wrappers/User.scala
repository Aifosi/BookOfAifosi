package bookofaifosi.wrappers

import java.net.URL

import cats.effect.IO
import bookofaifosi.syntax.action.*
import net.dv8tion.jda.api.entities.User as JDAUser
import compiletime.asMatchable
import scala.jdk.CollectionConverters.*

class User(private[wrappers] val user: JDAUser):
  lazy val id: Long = user.getIdLong
  lazy val mention: String = s"<@!$id>"
  lazy val name: String = user.getName
  def getNameIn(guild: Guild): String = guild.getMember(this).flatMap(_.nickname).getOrElse(name)
  lazy val avatarURL: URL = new URL(user.getAvatarUrl)
  lazy val isBot: Boolean = user.isBot

  lazy val privateChannel: IO[PrivateChannel] = user.openPrivateChannel.toIO.map(new PrivateChannel(_))

  def sendMessage(message: String): IO[Message] = privateChannel.flatMap(_.sendMessage(message))

  def voiceChannel: Option[VoiceChannel] = user.getJDA.getVoiceChannels.asScala.toList.collectFirst {
    case channel if channel.getMembers.asScala.exists(_.getUser.getIdLong == user.getIdLong) =>
      new VoiceChannel(channel)
  }

  def member: Option[Member] = voiceChannel.flatMap(_.members.find(_.id == id))

  def isSelfMuted: Option[Boolean] = member.map(_.isSelfMuted)

  override def toString: String = s"$name($id)"

  def canEqual(other: Any): Boolean = other.isInstanceOf[User]

  override def equals(other: Any): Boolean = other.asMatchable match
    case that: User =>
      that.canEqual(this) &&
        id == that.id
    case _ => false

  override def hashCode(): Int =
    val state = Seq(id)
    state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
