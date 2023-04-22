package bot.model

import bot.commands.SlashPattern
import bot.model.Discord.*
import bot.syntax.action.*
import bot.syntax.io.*
import bot.syntax.task.toIO as asIO
import bot.utils.Maybe

import cats.data.EitherT
import cats.effect.IO
import fs2.Stream
import net.dv8tion.jda.api.entities.Guild as JDAGuild
import net.dv8tion.jda.api.entities.User as JDAUser
import net.dv8tion.jda.api.interactions.commands.Command as JDACommand
import net.dv8tion.jda.api.interactions.commands.build.SlashCommandData
import scala.jdk.CollectionConverters.*

class Guild(private[model] val guild: JDAGuild):
  lazy val discordID: DiscordID = guild.getIdLong
  lazy val ownerID: DiscordID   = guild.getOwnerIdLong

  def isOwner(user: User): Boolean = user.discordID == ownerID

  def isOwner(member: Member): Boolean = member.discordID == ownerID

  def roles: List[Role] = guild.getRoles.asScala.toList.map(new Role(_))

  def members: Stream[IO, Member] = guild.loadMembers().asIO.map(_.asScala.map(new Member(_))).streamedIterable

  def member(user: User): Maybe[Member] =
    actionGetter[JDAUser](user.user, "guild member", guild.retrieveMember, new Member(_))

  def member(userDiscordID: DiscordID): Maybe[Member] =
    actionGetter[Long](userDiscordID.toLong, "guild member", guild.retrieveMemberById, new Member(_))

  def addCommands(commands: List[SlashCommandData]): IO[List[DiscordID]] =
    guild
      .updateCommands()
      .addCommands(commands*)
      .toIO
      .map(_.asScala.toList.map(command => DiscordID(command.getIdLong)))

  def commands: IO[List[JDACommand]] = guild.retrieveCommands().toIO.map(_.asScala.toList)
