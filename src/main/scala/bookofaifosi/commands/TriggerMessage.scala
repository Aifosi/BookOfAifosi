package bookofaifosi.commands

import bookofaifosi.model.{Channel, User}
import bookofaifosi.model.event.SlashCommandEvent
import cats.data.{OptionT, EitherT}
import cats.effect.IO
import cats.syntax.foldable.*

object TriggerMessage extends SlashCommand with Options:
  override val defaultEnabled: Boolean = false
  override val fullCommand: String = "trigger"
  override val options: List[PatternOption] = List(
    _.addOption[User]("user", "Who do you want to send the message to."),
    _.addOption[Channel]("channel", "Channel with the message you want to send."),
  )

  override def apply(pattern: SlashPattern, event: SlashCommandEvent): IO[Boolean] =
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
