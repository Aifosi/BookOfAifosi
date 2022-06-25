package bookofaifosi

import cats.effect.IO
import bookofaifosi.commands.{Command, ReactionCommand, SlashCommand, TextCommand, AutoCompletable}
import bookofaifosi.model.event.ReactionEvent.*
import bookofaifosi.model.event.ReactionEvent.given
import bookofaifosi.model.event.MessageEvent.*
import bookofaifosi.model.event.MessageEvent.given
import bookofaifosi.model.event.SlashCommandEvent.*
import bookofaifosi.model.event.SlashCommandEvent.given
import bookofaifosi.model.event.{AutoCompleteEvent, Event, MessageEvent, ReactionEvent, SlashCommandEvent}
import bookofaifosi.syntax.logger.*
import bookofaifosi.syntax.io.*
import net.dv8tion.jda.api.events.message.MessageReceivedEvent
import net.dv8tion.jda.api.events.message.react.MessageReactionAddEvent
import net.dv8tion.jda.api.events.interaction.command.{CommandAutoCompleteInteractionEvent, SlashCommandInteractionEvent}
import net.dv8tion.jda.api.hooks.ListenerAdapter

object MessageListener extends ListenerAdapter:
  private def runCommandList[T, E <: Event](
    event: E,
    commands: List[Command[T, E]],
  )(
    log: (E, Command[T, E]) => IO[Unit],
  ): IO[Unit] =
    if !event.author.isBot then
      commands
        .sortBy(-_.pattern.toString.length)
        .foldLeft(IO.pure(false)) {
          case (io, command) if command.matches(event) =>
            for
              stopped <- io.logError(true)
              stop <-
                if stopped then
                  IO.pure(true)
                else
                  log(event, command) *> command.apply(command.pattern, event)
            yield stop
          case (io, _) => io
        }
        .as(())
    else
      IO.unit

  override def onMessageReceived(event: MessageReceivedEvent): Unit =
    runCommandList(event, Bot.textCommands) { (event, command) =>
      lazy val subgroups = command.pattern.findFirstMatchIn(event.content).get.subgroups.mkString(" ")
      if command.pattern != Command.all then
        Bot.logger.info(s"${event.author} issued text command $command $subgroups".trim)
      else
        IO.unit
    }.unsafeRunSync()(Bot.ioRuntime)

  override def onMessageReactionAdd(event: MessageReactionAddEvent): Unit =
    runCommandList(event, Bot.reactionCommands) { (event, command) =>
      Bot.logger.info(s"${event.author} issued reaction command $command".trim)
    }.unsafeRunSync()(Bot.ioRuntime)

  override def onSlashCommandInteraction(event: SlashCommandInteractionEvent): Unit =
    runCommandList(event, Bot.slashCommands) { (event, command) =>
      Bot.logger.info(s"${event.author} issued slash command $command".trim)
    }.unsafeRunSync()(Bot.ioRuntime)

  override def onCommandAutoCompleteInteraction(event: CommandAutoCompleteInteractionEvent): Unit =
    Bot.autoCompletableCommands.foldLeft(IO.pure(false)) {
      case (io, command) if command.matchesAutoComplete(event) =>
        for
          stopped <- io
          stop <- if stopped then IO.pure(true) else command.apply(event)
        yield stop
      case (io, _) => io
    }
      .as(())
      .unsafeRunSync()(Bot.ioRuntime)
