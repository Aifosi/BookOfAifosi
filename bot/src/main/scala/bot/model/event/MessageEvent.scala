package bot.model.event

import bot.model.Message
import bot.model.event.{Event, GenericTextEvent}

import cats.effect.IO
import java.util.concurrent.{CancellationException, CompletableFuture, CompletionException}
import net.dv8tion.jda.api.entities.{
  Guild as JDAGuild,
  Member as JDAMember,
  Message as JDAMessage,
  MessageChannel,
  User as JDAUser,
}
import net.dv8tion.jda.api.events.message.MessageReceivedEvent
import scala.util.Try

class MessageEvent(
  jdaMessage: JDAMessage,
  jdaChannel: MessageChannel,
  jdaAuthor: JDAUser,
  jdaMember: Option[JDAMember],
  jdaGuild: Option[JDAGuild],
) extends GenericTextEvent(jdaChannel, jdaAuthor, jdaMember, jdaGuild):
  lazy val message: Message = new Message(jdaMessage)
  lazy val content: String  = message.content

object MessageEvent:
  given Conversion[MessageReceivedEvent, MessageEvent] = event =>
    new MessageEvent(
      event.getMessage,
      event.getChannel,
      event.getAuthor,
      Option(event.getMember),
      Try(event.getGuild).toOption,
    )
