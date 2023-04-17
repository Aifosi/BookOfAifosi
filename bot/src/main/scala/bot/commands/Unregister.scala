package bot.commands

import bot.Registration
import bot.commands.{SlashCommand, SlashPattern}
import bot.model.event.SlashCommandEvent
import cats.effect.IO
import org.typelevel.log4cats.Logger

class Unregister(registration: Registration) extends SlashCommand:
  override val isUserCommand: Boolean = true
  override val fullCommand: String = "unregister"

  override def apply(pattern: SlashPattern, event: SlashCommandEvent)(using Logger[IO]): IO[Boolean] =
    for
      member <- event.authorMember
      message <- registration.unregister(member)
      _ <- message.fold(IO.unit)(event.replyEphemeral)
    yield true

  override val description: String = "Unlink your discord and chaster"
