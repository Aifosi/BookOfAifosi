package bookofaifosi.commands

import bookofaifosi.Registration
import bookofaifosi.Registration.Role
import bookofaifosi.model.event.SlashCommandEvent
import cats.effect.IO

import scala.concurrent.duration.*

object RegisterWearer extends SlashCommand:
  override val defaultEnabled: Boolean = true

  override val fullCommand: String = "register wearer"

  override def apply(pattern: SlashPattern, event: SlashCommandEvent): IO[Boolean] =
    val timeout = 10.minutes
    for
      authorMember <- event.authorMember
      uri <- Registration.register(authorMember, timeout, Role.Wearer)
      _ <- event.replyEphemeral(s"To complete registration please visit $uri, this link expires in $timeout")
    yield true

  override val description: String = "Register with Book of Aifosi as wearer."
