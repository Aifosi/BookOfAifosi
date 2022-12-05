package bot

import cats.effect.IO
import bot.commands.{AutoCompletable, Command, NoLog, ReactionCommand, SlashCommand, TextCommand}
import bot.model.{User, Member}
import bot.model.event.ReactionEvent.*
import bot.model.event.ReactionEvent.given
import bot.model.event.MessageEvent.*
import bot.model.event.MessageEvent.given
import bot.model.event.SlashCommandEvent.*
import bot.model.event.SlashCommandEvent.given
import bot.model.event.{AutoCompleteEvent, Event, MessageEvent, ReactionEvent, SlashCommandEvent}
import bot.syntax.io.*
import cats.effect.unsafe.IORuntime
import net.dv8tion.jda.api.events.guild.member.GuildMemberRemoveEvent
import net.dv8tion.jda.api.events.message.MessageReceivedEvent
import net.dv8tion.jda.api.events.message.react.MessageReactionAddEvent
import net.dv8tion.jda.api.events.interaction.command.{CommandAutoCompleteInteractionEvent, SlashCommandInteractionEvent}
import net.dv8tion.jda.api.hooks.ListenerAdapter
import net.dv8tion.jda.api.interactions.commands.OptionType
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger

class MessageListener(bot: Bot)(using Logger[IO], IORuntime) extends ListenerAdapter:
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
                  for
                    _ <- if command.isInstanceOf[NoLog] then IO.unit else log(event, command)
                    r <- command.apply(command.pattern, event)
                  yield r
            yield stop
          case (io, _) => io
        }
        .void
    else
      IO.unit

  private def log(event: Event, message: String): IO[Unit] =
    for
      guild <- event.guild
      mention = if guild.isOwner(event.author) then event.author.name else event.author.mention
      _ <- Bot.channels.log.sendMessage(mention + message).value.void //TODO Add to base bot config, maybe on same namespace?
      _ <- Logger[IO].info(event.author.toString + message)
    yield ()

  override def onMessageReceived(event: MessageReceivedEvent): Unit =
    runCommandList(event, bot.textCommands) { (event, command) =>
      lazy val subgroups = command.pattern.findFirstMatchIn(event.content).get.subgroups.mkString(" ")
      if command.pattern != Command.all then
        log(event, s" issued text command $command $subgroups".stripTrailing)
      else
        IO.unit
    }.unsafeRunSync()

  override def onMessageReactionAdd(event: MessageReactionAddEvent): Unit =
    runCommandList(event, bot.reactionCommands) { (event, command) =>
      log(event, s" issued reaction command $command".stripTrailing)
    }.unsafeRunSync()

  override def onSlashCommandInteraction(event: SlashCommandInteractionEvent): Unit =
    runCommandList(event, bot.slashCommands) { (event, command) =>
      val options = event.allOptions.map {
        case option if option.getType == OptionType.MENTIONABLE || option.getType == OptionType.USER || option.getType == OptionType.ROLE || option.getType == OptionType.CHANNEL =>
          s"${option.getName}: ${option.getAsMentionable.getAsMention}"
        case option =>
          s"${option.getName}: ${option.getAsString}"
      }.mkString(", ")
      log(event, s" issued slash command $command${if options.nonEmpty then s", options: $options" else ""}".stripTrailing)
    }.unsafeRunSync()

  override def onCommandAutoCompleteInteraction(event: CommandAutoCompleteInteractionEvent): Unit =
    bot.autoCompletableCommands.foldLeft(IO.pure(false)) {
      case (io, command) if command.matchesAutoComplete(event) =>
        for
          stopped <- io
          stop <- if stopped then IO.pure(true) else command.apply(event)
        yield stop
      case (io, _) => io
    }
      .void
      .unsafeRunSync()

  override def onGuildMemberRemove(event: GuildMemberRemoveEvent): Unit =
    val io = for
      logger  <- Slf4jLogger.create[IO]
      message <- Registration.unregister(new Member(event.getMember))
      _ <- message.fold(IO.unit)(new User(event.getUser).sendMessage)
    yield ()
    io.unsafeRunSync()
