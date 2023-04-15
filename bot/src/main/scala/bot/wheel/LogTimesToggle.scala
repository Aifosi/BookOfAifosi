package bot.wheel

import bot.DiscordLogger
import bot.chaster.{ChasterClient, Lock}
import bot.db.RegisteredUserRepository
import bot.model.RegisteredUser
import bot.tasks.TextWheelCommand
import cats.effect.IO
import org.typelevel.log4cats.Logger

import scala.util.matching.Regex

class LogTimesToggle(
  client: ChasterClient,
  registeredUserRepository: RegisteredUserRepository,
)(using discordLogger: DiscordLogger) extends TextWheelCommand(client, registeredUserRepository):
  override lazy val pattern: Regex = "(Toggle|Show/Hide)LogTimes".r
  override def run(user: RegisteredUser, lock: Lock, text: String)(using Logger[IO]): IO[Boolean] =
    client.authenticatedEndpoints(user.token)
      .updateSettings(lock._id, settings => settings.copy(hideTimeLogs = !settings.hideTimeLogs))
      .as(true)

  override val description: String = "Toggles time being shown in the lock log"
