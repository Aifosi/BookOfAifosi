package bookofaifosi.commands

import bookofaifosi.db.RegisteredUserRepository
import bookofaifosi.model.User
import bookofaifosi.model.event.SlashCommandEvent
import cats.effect.IO
import cats.syntax.traverse.*
import cats.instances.option.*
import org.typelevel.log4cats.Logger
import bookofaifosi.db.Filters.*

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
