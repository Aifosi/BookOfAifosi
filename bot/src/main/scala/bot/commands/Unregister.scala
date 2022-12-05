package bot.commands

import bot.Registration
import bot.commands.{SlashCommand, SlashPattern}
import bot.model.event.SlashCommandEvent
import cats.effect.IO
import org.typelevel.log4cats.Logger

object Unregister extends SlashCommand:
  override val defaultEnabled: Boolean = true
  override val fullCommand: String = "unregister"

  override def apply(pattern: SlashPattern, event: SlashCommandEvent)(using Logger[IO]): IO[Boolean] =
    event.authorMember.flatMap(Registration.unregister).as(true)

  override val description: String = "Unlink your discord and chaster"
