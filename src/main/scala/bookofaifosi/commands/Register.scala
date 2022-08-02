package bookofaifosi.commands

import bookofaifosi.Registration
import bookofaifosi.model.event.SlashCommandEvent
import cats.effect.IO
import scala.concurrent.duration.*
import org.typelevel.log4cats.Logger

object Register extends SlashCommand:
  override val defaultEnabled: Boolean = true

  override val fullCommand: String = "register"

  override def apply(pattern: SlashPattern, event: SlashCommandEvent)(using Logger[IO]): IO[Boolean] =
    val timeout = 10.minutes
    for
      authorMember <- event.authorMember
      uri <- Registration.register(authorMember, timeout)
      message = uri.fold("You are already registered.")(uri => s"To complete registration please visit $uri, this link expires in $timeout")
      _ <- event.replyEphemeral(message)
    yield true

  override val description: String = "Allows you to register, linking your discord and chaster accounts."
