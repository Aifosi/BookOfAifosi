package bookofaifosi.model.event

import bookofaifosi.Bot
import bookofaifosi.model.{DiscordID, Message}
import cats.effect.IO
import bookofaifosi.syntax.all.*
import bookofaifosi.model.toLong
import net.dv8tion.jda.api.entities.{MessageChannel, Guild as JDAGuild, Member as JDAMember, User as JDAUser}
import net.dv8tion.jda.api.events.message.react.MessageReactionAddEvent

class ReactionEvent(
  val content: String,
  jdaChannel: MessageChannel,
  jdaAuthor: JDAUser,
  jdaMember: Option[JDAMember],
  val messageID: DiscordID,
  jdaGuild: Option[JDAGuild],
) extends Event(jdaChannel, jdaAuthor, jdaMember, jdaGuild):

  def addReaction(emoji: String): IO[Unit] =
    for
      message <- jdaChannel.retrieveMessageById(messageID.toLong).toIO
      _ <- message.addReaction(emoji).toIO
    yield ()

  lazy val message: IO[Message] =
    channel.findMessageByID(messageID)
      .getOrRaise(new Exception(s"Message $messageID not found in channel ${channel.discordID}"))


object ReactionEvent:
  given Conversion[MessageReactionAddEvent, ReactionEvent] = event =>
    new ReactionEvent(
      event.getReactionEmote.getEmoji,
      event.getChannel,
      event.getUser,
      Option(event.getMember),
      DiscordID(event.getMessageIdLong),
      Option(event.getGuild),
    )
