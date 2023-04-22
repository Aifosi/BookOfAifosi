package bot.model

import bot.model.Discord.*
import bot.syntax.action.*
import bot.utils.Maybe

import cats.data.{EitherT, OptionT}
import cats.effect.IO
import cats.syntax.either.*
import net.dv8tion.jda.api.JDA
import net.dv8tion.jda.api.entities.MessageChannel
import net.dv8tion.jda.api.requests.RestAction
import scala.jdk.CollectionConverters.*

class Discord(val jda: JDA):
  def userByID(id: DiscordID): Maybe[User] =
    actionGetter(id.toLong, "user", jda.retrieveUserById, new User(_))

  def guildByID(id: DiscordID): Maybe[Guild] =
    getter[Long](id.toLong, "guild", jda.getGuildById, new Guild(_))

  def roleByID(id: DiscordID): Maybe[Role] =
    getter[Long](id.toLong, "role", jda.getRoleById, new Role(_))

  def channelByID(id: DiscordID): Maybe[Channel] =
    getter[Long](id.toLong, "channel", jda.getChannelById(classOf[MessageChannel], _), new Channel(_))

  def roles(guildID: DiscordID): Maybe[List[Role]] = guildByID(guildID.toLong).map(_.roles)

  def unsafeRoleByID(id: DiscordID): IO[Role] = roleByID(id).rethrowT

  def guilds: List[Guild] = jda.getGuilds.asScala.toList.map(new Guild(_))

object Discord:
  final private[model] class PartiallyAppliedGetter[ID](private val dummy: Boolean = true) extends AnyVal:
    def apply[A, B](id: ID, what: String, get: ID => A, transform: A => B): Maybe[B] =
      EitherT.fromOption(Option(get(id)), new Exception(s"Failed to get $what with id $id")).map(transform)

  def getter[ID]: PartiallyAppliedGetter[ID] = new PartiallyAppliedGetter[ID]

  final private[model] class PartiallyAppliedActionGetter[ID](private val dummy: Boolean = true) extends AnyVal:
    def apply[A, B](id: ID, what: String, get: ID => RestAction[A], transform: A => B): Maybe[B] =
      EitherT(get(id).toIO.attempt.map(_.leftMap(_ => new Exception(s"Failed to get $what using: $id")))).map(transform)

  def actionGetter[ID]: PartiallyAppliedActionGetter[ID] = new PartiallyAppliedActionGetter[ID]
