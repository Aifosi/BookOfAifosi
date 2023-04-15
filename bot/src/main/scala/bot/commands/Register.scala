package bot.commands

import bot.Registration
import bot.model.event.SlashCommandEvent
import cats.effect.IO
import scala.concurrent.duration.*
import org.typelevel.log4cats.Logger

class Register(registration: Registration) extends SlashCommand:
  override val isUserCommand: Boolean = true

  override val fullCommand: String = "register"

  override def apply(pattern: SlashPattern, event: SlashCommandEvent)(using Logger[IO]): IO[Boolean] =
    val timeout = 10.minutes
    for
      authorMember <- event.authorMember
      uri <- registration.register(authorMember, timeout)
      message = uri.fold("You are already registered.")(uri => s"To complete registration please visit $uri, this link expires in $timeout")
      _ <- event.replyEphemeral(message)
    yield true

  override val description: String = "Allows you to register, linking your discord and chaster accounts"
