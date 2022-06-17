package bookofaifosi

import cats.effect.IO
import bookofaifosi.commands.{Command, ReactionCommand, SlashCommand, TextCommand}
import bookofaifosi.wrappers.event.ReactionEvent.*
import bookofaifosi.wrappers.event.ReactionEvent.given
import bookofaifosi.wrappers.event.MessageEvent.*
import bookofaifosi.wrappers.event.MessageEvent.given
import bookofaifosi.wrappers.event.SlashCommandEvent.*
import bookofaifosi.wrappers.event.SlashCommandEvent.given
import bookofaifosi.wrappers.event.Event
import net.dv8tion.jda.api.events.message.MessageReceivedEvent
import net.dv8tion.jda.api.events.message.react.MessageReactionAddEvent
import net.dv8tion.jda.api.events.interaction.command.SlashCommandInteractionEvent
import net.dv8tion.jda.api.hooks.ListenerAdapter
import cats.effect.unsafe.implicits.global
import bookofaifosi.wrappers.event.{Event, MessageEvent, ReactionEvent, SlashCommandEvent}

object MessageListener extends ListenerAdapter:
  out =>

  private def runCommandList[T, E <: Event](
    event: E,
    commands: List[Command[T, E]],
    privateChannel: Boolean,
  )(
    log: (E, Command[T, E]) => IO[Unit],
  ): IO[Unit] =
    if !event.author.isBot then
      commands
        .sortBy(-_.pattern.toString.length)
        .foldLeft(IO.pure(false)) {
          case (io, command) if command.matches(event) =>
            for
              stopped <- io
              stop <-
                if stopped then
                  IO.pure(true)
                else
                  log(event, command) *> command.run(command.pattern, event, privateChannel)
            yield stop
          case (io, _) => io
        }
        .as(())
    else
      IO.unit

  private def runTextCommandList(
    event: MessageEvent,
    commands: List[TextCommand],
    privateChannel: Boolean,
  ): IO[Unit] =
    runCommandList(event, commands, privateChannel) { (event, command) =>
      lazy val subgroups = command.pattern.findFirstMatchIn(event.content).get.subgroups.mkString(" ")
      if command.pattern != Command.all then
        IO.println(s"${event.author} issued text command $command $subgroups".trim)
      else
        IO.unit
    }

  private def runReactionCommandList(event: ReactionEvent, commands: List[ReactionCommand], privateChannel: Boolean): IO[Unit] =
    runCommandList(event, commands, privateChannel) { (event, command) =>
      IO.println(s"${event.author} issued reaction command $command".trim)
    }

  private def runSlashCommandList(
    event: SlashCommandEvent,
    commands: List[SlashCommand],
    privateChannel: Boolean,
  ): IO[Unit] =
    runCommandList(event, commands, privateChannel) { (event, command) =>
      IO.println(s"${event.author} issued slash command $command".trim)
    }

  override def onMessageReceived(event: MessageReceivedEvent): Unit =
    runTextCommandList(event, Bot.textCommands, privateChannel = false).unsafeRunSync()

  override def onMessageReactionAdd(event: MessageReactionAddEvent): Unit =
    runReactionCommandList(event, Bot.reactionCommands, privateChannel = false).unsafeRunSync()

  override def onSlashCommandInteraction(event: SlashCommandInteractionEvent): Unit =
    runSlashCommandList(event, Bot.slashCommands, privateChannel = false).unsafeRunSync()
