package bookofaifosi.model.event

import cats.effect.IO
import bookofaifosi.model.event.{Event, GenericTextEvent}
import bookofaifosi.model.Message
import net.dv8tion.jda.api.entities.{MessageChannel, Guild as JDAGuild, Member as JDAMember, Message as JDAMessage, User as JDAUser}
import net.dv8tion.jda.api.events.message.MessageReceivedEvent

import java.util.concurrent.{CancellationException, CompletableFuture, CompletionException}
import scala.util.Try

class MessageEvent(
  jdaMessage: JDAMessage,
  jdaChannel: MessageChannel,
  jdaAuthor: JDAUser,
  jdaMember: Option[JDAMember],
  jdaGuild: Option[JDAGuild],
) extends GenericTextEvent(jdaChannel, jdaAuthor, jdaMember, jdaGuild):
  lazy val message: Message = new Message(jdaMessage)
  lazy val content: String = message.content

object MessageEvent:
  given Conversion[MessageReceivedEvent, MessageEvent] = event =>
    new MessageEvent(
      event.getMessage,
      event.getChannel,
      event.getAuthor,
      Option(event.getMember),
      Try(event.getGuild).toOption,
    )
