package lurch.commands

import bot.commands.{Options, PatternOption, SlashCommand, SlashPattern}
import bot.db.Filters.*
import bot.db.RegisteredUserRepository
import bot.model.User
import bot.model.event.SlashCommandEvent
import cats.effect.IO
import cats.instances.option.*
import cats.syntax.traverse.*
import org.typelevel.log4cats.Logger

object Info extends SlashCommand with Options {
  override val options: List[PatternOption] = List(
    _.addOption[Option[User]]("user", "Discord user mention to get data for."),
  )
  override val defaultEnabled: Boolean = true
  override val fullCommand: String = "info"

  override def apply(pattern: SlashPattern, event: SlashCommandEvent)(using Logger[IO]): IO[Boolean] =
    val user = event.getOption[Option[User]]("user")

    user.map(_.discordID).flatTraverse { discordID =>
      RegisteredUserRepository.find(discordID.equalDiscordID)
    }
    ???


  override val description: String = "Show some info about the user"
}
