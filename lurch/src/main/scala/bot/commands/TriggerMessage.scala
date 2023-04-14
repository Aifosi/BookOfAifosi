package bot.commands

import bot.commands.{Options, SlashCommand, SlashPattern, PatternOption}
import bot.model.event.SlashCommandEvent
import bot.model.{Channel, User}
import cats.data.{EitherT, OptionT}
import cats.effect.IO
import cats.syntax.foldable.*
import org.typelevel.log4cats.Logger

object TriggerMessage extends SlashCommand with Options:
  override val defaultEnabled: Boolean = false
  override val fullCommand: String = "trigger"
  override val options: List[PatternOption] = List(
    _.addOption[User]("user", "Who do you want to send the message to."),
    _.addOption[Channel]("channel", "Channel with the message you want to send."),
  )

  override def apply(pattern: SlashPattern, event: SlashCommandEvent)(using Logger[IO]): IO[Boolean] =
    val user = event.getOption[User]("user")
    val channel = event.getOption[Channel]("channel")
    (for
      lastMessages <- EitherT(channel.lastHundred.attempt).leftMap(_ => s"Failed to get message from $channel")
      _ <- EitherT.liftF(lastMessages.traverse_(user.sendMessage))
    yield true).foldF(
      error => event.replyEphemeral(error),
      _ => event.replyEphemeral("Message sent"),
    )
    .as(true)


  override val description: String = "Sends the last message in a channel to a user."
