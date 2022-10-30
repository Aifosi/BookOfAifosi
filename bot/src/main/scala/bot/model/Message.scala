package bot.model

import cats.effect.IO
import bot.syntax.action.*
import cats.instances.list.*
import cats.syntax.foldable.*
import net.dv8tion.jda.api.entities.Message as JDAMessage

class Message(private[model] val message: JDAMessage):
  lazy val content: String = message.getContentRaw
  lazy val id: DiscordID = message.getIdLong
  lazy val jumpUrl: String = message.getJumpUrl

  def addReactions(reactions: String*): IO[Unit] =
    reactions.toList.traverse_(reaction => message.addReaction(reaction).toIO)
  def addReaction(reaction: String): IO[Unit] = message.addReaction(reaction).toIO.void
  def removeUserReaction(reaction: String, user: User): IO[Unit] = message.removeReaction(reaction, user.user).toIO.void
  def edit(string: String): IO[Message] = message.editMessage(string).toIO.map(new Message(_))
  def delete: IO[Unit] = message.delete().toIO.void
