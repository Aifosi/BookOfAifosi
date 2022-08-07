package bookofaifosi.model

import cats.effect.IO
import bookofaifosi.syntax.action.*
import cats.instances.list.*
import cats.syntax.foldable.*
import net.dv8tion.jda.api.entities.Message as JDAMessage

class Message(private[model] val message: JDAMessage):
  lazy val content: String = message.getContentRaw

  def addReactions(reactions: String*): IO[Unit] =
    reactions.toList.traverse_(reaction => message.addReaction(reaction).toIO)
  def addReaction(string: String): IO[Unit] = message.addReaction(string).toIO.void
  def edit(string: String): IO[Message] = message.editMessage(string).toIO.map(new Message(_))
  def delete: IO[Unit] = message.delete().toIO.void
