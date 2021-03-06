package bookofaifosi.model

import bookofaifosi.commands.SlashPattern
import net.dv8tion.jda.api.entities.Guild as JDAGuild
import bookofaifosi.syntax.action.*
import cats.effect.IO
import net.dv8tion.jda.api.interactions.commands.build.SlashCommandData

import scala.jdk.CollectionConverters.*

class Guild(private[model] val guild: JDAGuild):
  lazy val discordID: DiscordID = guild.getIdLong
  lazy val ownerID: DiscordID = guild.getOwnerIdLong
  def isOwner(user: User): Boolean = user.discordID == ownerID
  def isOwner(member: Member): Boolean = member.discordID == ownerID
  def roles: List[Role] = guild.getRoles.asScala.toList.map(new Role(_))
  def getMember(user: User): IO[Member] = guild.retrieveMember(user.user).toIO.map(new Member(_))
  def addCommands(commands: List[SlashCommandData]): IO[Unit] = guild.updateCommands().addCommands(commands*).toIO.void
