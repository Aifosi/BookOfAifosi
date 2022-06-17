package bookofaifosi.wrappers

import cats.effect.IO
import bookofaifosi.syntax.action.*
import net.dv8tion.jda.api.entities.Member as JDAMember

import scala.jdk.CollectionConverters.*

class Member(member: JDAMember):
  lazy val id: Long = member.getIdLong

  lazy val user: User = new User(member.getUser)

  lazy val nickname: Option[String] = Option(member.getNickname)

  def isSelfMuted: Boolean = member.getVoiceState.isSelfMuted

  def isGuildMuted: Boolean = member.getVoiceState.isGuildMuted

  def mute: IO[Unit] = member.mute(true).toIO.as(())

  def unmute: IO[Unit] = member.mute(false).toIO.as(())

  def toggleMute: IO[Unit] = member.mute(!isGuildMuted).toIO.as(())

  def roles: Set[Role] = member.getRoles.asScala.toSet.map(new Role(_))

  def hasRole(role: Role): Boolean = roles.exists(_.id == role.id)

  def isGuildOwner: Boolean = member.getGuild.getOwnerIdLong == id
