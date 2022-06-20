package bookofaifosi.model.event

import cats.effect.IO
import bookofaifosi.syntax.all.*
import bookofaifosi.model.event.{Event, ReactionEvent}
import net.dv8tion.jda.api.entities.{MessageChannel, Guild as JDAGuild, Member as JDAMember, User as JDAUser}
import net.dv8tion.jda.api.events.message.react.MessageReactionAddEvent

class ReactionEvent(
  val content: String,
  jdaChannel: MessageChannel,
  jdaAuthor: JDAUser,
  jdaMember: Option[JDAMember],
  messageID: Long,
  jdaGuild: Option[JDAGuild],
) extends Event(jdaChannel, jdaAuthor, jdaMember, jdaGuild):

  def addReaction(emoji: String): IO[Unit] =
    for
      message <- jdaChannel.retrieveMessageById(messageID).toIO
      _ <- message.addReaction(emoji).toIO
    yield ()

object ReactionEvent:
  given Conversion[MessageReactionAddEvent, ReactionEvent] = event =>
    new ReactionEvent(
      event.getReactionEmote.getEmoji,
      event.getChannel,
      event.getUser,
      Option(event.getMember),
      event.getMessageIdLong,
      Option(event.getGuild),
    )
