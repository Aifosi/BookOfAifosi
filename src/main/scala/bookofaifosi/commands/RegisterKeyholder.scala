package bookofaifosi.commands

import bookofaifosi.Registration
import bookofaifosi.Registration.Role
import bookofaifosi.model.event.SlashCommandEvent
import cats.effect.IO
import scala.concurrent.duration.*
import org.typelevel.log4cats.Logger

object RegisterKeyholder extends SlashCommand:
  override val defaultEnabled: Boolean = true

  override val fullCommand: String = "register keyholder"

  override def apply(pattern: SlashPattern, event: SlashCommandEvent)(using Logger[IO]): IO[Boolean] =
    val timeout = 10.minutes
    for
      authorMember <- event.authorMember
      uri <- Registration.register(authorMember, timeout, Role.Keyholder)
      message = uri.fold("You are already registered as a keyholder.")(uri => s"To complete registration please visit $uri, this link expires in $timeout")
      _ <- event.replyEphemeral(message)
    yield true

  override val description: String = "Register with Book of Aifosi as keyholder."
