package bookofaifosi.model

import net.dv8tion.jda.api.entities.Guild as JDAGuild

import scala.jdk.CollectionConverters.*

class Guild(guild: JDAGuild):
  lazy val ownerID: Long = guild.getOwnerIdLong
  def isOwner(user: User): Boolean = user.discordID == ownerID
  def isOwner(member: Member): Boolean = member.id == ownerID
  def roles: List[Role] = guild.getRoles.asScala.toList.map(new Role(_))
  def getMember(user: User): Option[Member] = Option(guild.getMember(user.user)).map(new Member(_))
