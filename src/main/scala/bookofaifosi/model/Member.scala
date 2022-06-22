package bookofaifosi.model

import cats.effect.IO
import cats.syntax.functor.*
import bookofaifosi.syntax.action.*
import net.dv8tion.jda.api.entities.Member as JDAMember

import scala.jdk.CollectionConverters.*

class Member(member: JDAMember) extends User(member.getUser):
  lazy val guild: Guild = new Guild(member.getGuild)
  
  lazy val effectiveName: String = member.getEffectiveName

  def isSelfMuted: Boolean = member.getVoiceState.isSelfMuted

  def isGuildMuted: Boolean = member.getVoiceState.isGuildMuted

  def mute: IO[Unit] = member.mute(true).toIO.void

  def unmute: IO[Unit] = member.mute(false).toIO.void

  def toggleMute: IO[Unit] = member.mute(!isGuildMuted).toIO.void

  def roles: Set[Role] = member.getRoles.asScala.toSet.map(new Role(_))

  def hasRole(role: Role): Boolean = roles.exists(_.discordID == role.discordID)

  def addRole(role: Role): IO[Unit] = member.getGuild.addRoleToMember(member, role.role).toIO.void

  def removeRole(role: Role): IO[Unit] = member.getGuild.removeRoleFromMember(member, role.role).toIO.void

  def isGuildOwner: Boolean = DiscordID(member.getGuild.getOwnerIdLong) == discordID
