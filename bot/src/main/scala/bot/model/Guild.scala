package bot.model

import bot.commands.SlashPattern
import net.dv8tion.jda.api.entities.Guild as JDAGuild
import bot.syntax.action.*
import bot.syntax.task.toIO as asIO
import bot.syntax.io.*
import cats.effect.IO
import net.dv8tion.jda.api.interactions.commands.build.SlashCommandData
import fs2.Stream

import scala.jdk.CollectionConverters.*

class Guild(private[model] val guild: JDAGuild):
  lazy val discordID: DiscordID = guild.getIdLong
  lazy val ownerID: DiscordID = guild.getOwnerIdLong
  def isOwner(user: User): Boolean = user.discordID == ownerID
  def isOwner(member: Member): Boolean = member.discordID == ownerID
  def roles: List[Role] = guild.getRoles.asScala.toList.map(new Role(_))
  def members: Stream[IO, Member] = guild.loadMembers().asIO.map(_.asScala.map(new Member(_))).streamedIterable
  def getMember(user: User): IO[Member] = guild.retrieveMember(user.user).toIO.map(new Member(_))
  def getMember(userDiscordID: DiscordID): IO[Member] = guild.retrieveMemberById(userDiscordID.toLong).toIO.map(new Member(_))
  def addCommands(commands: List[SlashCommandData]): IO[Unit] = guild.updateCommands().addCommands(commands*).toIO.void