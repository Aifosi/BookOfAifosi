package bookofaifosi.wrappers.event

import bookofaifosi.commands.MacroHelper
import net.dv8tion.jda.api.entities.MessageChannel
import net.dv8tion.jda.api.events.interaction.command.CommandAutoCompleteInteractionEvent
import net.dv8tion.jda.api.entities.{MessageChannel, Guild as JDAGuild, Member as JDAMember, User as JDAUser}
import bookofaifosi.syntax.action.*
import cats.effect.IO

class AutoCompleteEvent(
  jdaChannel: MessageChannel,
  jdaAuthor: JDAUser,
  jdaMember: Option[JDAMember],
  jdaGuild: Option[JDAGuild],
  underlying: CommandAutoCompleteInteractionEvent,
) extends Event(jdaChannel, jdaAuthor, jdaMember, jdaGuild):
  def focusedOption: String = underlying.getFocusedOption.getName
  def focusedValue: String = underlying.getFocusedOption.getValue
  inline def replyChoices[T](options: List[T]): IO[Unit] = MacroHelper.replyChoices[T](underlying, options)
  lazy val commandName: String = underlying.getName
  lazy val subCommandName: Option[String] = Option(underlying.getSubcommandName)

object AutoCompleteEvent {
  given Conversion[CommandAutoCompleteInteractionEvent, AutoCompleteEvent] = event =>
    new AutoCompleteEvent(
      event.getMessageChannel,
      event.getUser,
      Option(event.getMember),
      Option(event.getGuild),
      event
    )
}
