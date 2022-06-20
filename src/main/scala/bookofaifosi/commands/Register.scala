package bookofaifosi.commands

import bookofaifosi.Registration
import bookofaifosi.model.event.SlashCommandEvent
import cats.effect.IO
import scala.concurrent.duration.*

object Register extends SlashCommand:
  override val defaultEnabled: Boolean = true

  override val fullCommand: String = "register"

  override def apply(pattern: SlashPattern, event: SlashCommandEvent): IO[Boolean] =
    val timeout = 10.minutes
    for
      uri <- Registration.basic(event.author, timeout)
      _ <- event.replyEphemeral(s"To complete registration please visit $uri, this link expires in $timeout")
    yield true

  override val description: String = "Register with Book of Aifosi"
