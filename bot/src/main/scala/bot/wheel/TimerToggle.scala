package bot.wheel

import bot.DiscordLogger
import bot.chaster.{ChasterClient, Lock}
import bot.db.RegisteredUserRepository
import bot.model.RegisteredUser
import bot.tasks.TextWheelCommand
import bot.syntax.kleisli.*
import bot.instances.functionk.given

import cats.effect.IO
import org.typelevel.log4cats.Logger
import scala.util.matching.Regex

class TimerToggle(
  client: ChasterClient,
  registeredUserRepository: RegisteredUserRepository,
)(using discordLogger: DiscordLogger)
    extends TextWheelCommand(client, registeredUserRepository):
  override lazy val pattern: Regex                                                                = "(Toggle|Show/Hide)Timer".r
  override def run(user: RegisteredUser, lock: Lock, text: String)(using Logger[IO]): IO[Boolean] =
    client
      .updateSettings(lock._id, settings => settings.copy(displayRemainingTime = !settings.displayRemainingTime))
      .runUsingTokenOf(user)
      .as(true)

  override val description: String = "Toggles the visibility of the lock timer"
