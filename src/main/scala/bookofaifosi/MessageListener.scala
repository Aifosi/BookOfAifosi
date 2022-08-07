package bookofaifosi

import cats.effect.IO
import bookofaifosi.commands.{AutoCompletable, Command, ReactionCommand, SlashCommand, TextCommand}
import bookofaifosi.model.User
import bookofaifosi.model.event.ReactionEvent.*
import bookofaifosi.model.event.ReactionEvent.given
import bookofaifosi.model.event.MessageEvent.*
import bookofaifosi.model.event.MessageEvent.given
import bookofaifosi.model.event.SlashCommandEvent.*
import bookofaifosi.model.event.SlashCommandEvent.given
import bookofaifosi.model.event.{AutoCompleteEvent, Event, MessageEvent, ReactionEvent, SlashCommandEvent}
import bookofaifosi.syntax.logger.*
import bookofaifosi.syntax.io.*
import cats.effect.unsafe.IORuntime
import net.dv8tion.jda.api.events.message.MessageReceivedEvent
import net.dv8tion.jda.api.events.message.react.MessageReactionAddEvent
import net.dv8tion.jda.api.events.interaction.command.{CommandAutoCompleteInteractionEvent, SlashCommandInteractionEvent}
import net.dv8tion.jda.api.hooks.ListenerAdapter
import org.typelevel.log4cats.Logger

class MessageListener(using Logger[IO], IORuntime) extends ListenerAdapter:
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
        .void
    else
      IO.unit

  private def log(event: Event, message: String): IO[Unit] =
    for
      logChannel <- Bot.config.logChannel
      _ <- logChannel.fold(IO.unit)(_.sendMessage(event.author.mention + message))
      _ <- Logger[IO].info(event.author.toString + message)
    yield ()

  override def onMessageReceived(event: MessageReceivedEvent): Unit =
    runCommandList(event, Bot.textCommands) { (event, command) =>
      lazy val subgroups = command.pattern.findFirstMatchIn(event.content).get.subgroups.mkString(" ")
      if command.pattern != Command.all then
        log(event, s" issued text command $command $subgroups".stripTrailing)
      else
        IO.unit
    }.unsafeRunSync()

  override def onMessageReactionAdd(event: MessageReactionAddEvent): Unit =
    runCommandList(event, Bot.reactionCommands) { (event, command) =>
      log(event, s" issued reaction command $command".stripTrailing)
    }.unsafeRunSync()

  override def onSlashCommandInteraction(event: SlashCommandInteractionEvent): Unit =
    runCommandList(event, Bot.slashCommands) { (event, command) =>
      log(event, s" issued slash command $command".stripTrailing)
    }.unsafeRunSync()

  override def onCommandAutoCompleteInteraction(event: CommandAutoCompleteInteractionEvent): Unit =
    Bot.autoCompletableCommands.foldLeft(IO.pure(false)) {
      case (io, command) if command.matchesAutoComplete(event) =>
        for
          stopped <- io
          stop <- if stopped then IO.pure(true) else command.apply(event)
        yield stop
      case (io, _) => io
    }
      .void
      .unsafeRunSync()
