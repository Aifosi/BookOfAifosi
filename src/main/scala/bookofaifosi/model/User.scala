package bookofaifosi.model

import java.net.URL

import cats.effect.IO
import cats.syntax.traverse.*
import bookofaifosi.syntax.action.*
import net.dv8tion.jda.api.entities.User as JDAUser
import compiletime.asMatchable
import scala.jdk.CollectionConverters.*

open class User(private[model] val user: JDAUser):
  lazy val discordID: DiscordID = user.getIdLong
  lazy val mention: String = user.getAsMention//s"<@!$discordID>"
  lazy val name: String = user.getName
  lazy val tag: String = user.getAsTag
  lazy val avatarURL: URL = new URL(user.getAvatarUrl)
  lazy val isBot: Boolean = user.isBot

  lazy val privateChannel: IO[PrivateChannel] = user.openPrivateChannel.toIO.map(new PrivateChannel(_))

  def sendMessage(message: String): IO[Message] = privateChannel.flatMap(_.sendMessage(message))

  def member(guild: Guild): IO[Member] = guild.getMember(this)

  def getNameIn(guild: Guild): IO[String] = member(guild).map(_.effectiveName)

  def addRole(guild: Guild, role: Role): IO[Unit] = member(guild).flatMap(_.addRole(role))

  def removeRole(guild: Guild, role: Role): IO[Unit] = member(guild).flatMap(_.removeRole(role))

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
