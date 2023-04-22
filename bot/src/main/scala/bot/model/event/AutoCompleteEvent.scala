package bot.model.event

import bot.commands.MacroHelper
import bot.syntax.action.*

import cats.effect.IO
import net.dv8tion.jda.api.entities.{Guild as JDAGuild, Member as JDAMember, MessageChannel, User as JDAUser}
import net.dv8tion.jda.api.entities.MessageChannel
import net.dv8tion.jda.api.events.interaction.command.CommandAutoCompleteInteractionEvent

class AutoCompleteEvent(
  jdaChannel: MessageChannel,
  jdaAuthor: JDAUser,
  jdaMember: Option[JDAMember],
  jdaGuild: Option[JDAGuild],
  underlying: CommandAutoCompleteInteractionEvent,
) extends Event(jdaChannel, jdaAuthor, jdaMember, jdaGuild):
  def focusedOption: String                              = underlying.getFocusedOption.getName
  def focusedValue: String                               = underlying.getFocusedOption.getValue
  inline def replyChoices[T](options: List[T]): IO[Unit] = MacroHelper.replyChoices[T](underlying, options)
  lazy val commandName: String                           = underlying.getName
  lazy val subCommandGroupName: Option[String]           = Option(underlying.getSubcommandGroup)
  lazy val subCommandName: Option[String]                = Option(underlying.getSubcommandName)
  lazy val fullCommand: String                           = List(Some(commandName), subCommandGroupName, subCommandName).flatten.mkString(" ")

object AutoCompleteEvent {
  given Conversion[CommandAutoCompleteInteractionEvent, AutoCompleteEvent] = event =>
    new AutoCompleteEvent(
      event.getMessageChannel,
      event.getUser,
      Option(event.getMember),
      Option(event.getGuild),
      event,
    )
}
