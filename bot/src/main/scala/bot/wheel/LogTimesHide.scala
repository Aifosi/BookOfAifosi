package bot.wheel

import bot.DiscordLogger
import bot.chaster.{ChasterClient, Lock}
import bot.db.RegisteredUserRepository
import bot.model.RegisteredUser
import bot.tasks.TextWheelCommand
import cats.effect.IO
import org.typelevel.log4cats.Logger

import scala.util.matching.Regex

class LogTimesHide(
  client: ChasterClient,
  registeredUserRepository: RegisteredUserRepository,
)(using discordLogger: DiscordLogger) extends TextWheelCommand(client, registeredUserRepository):
  override lazy val pattern: Regex = "HideLogTimes".r
  override def run(user: RegisteredUser, lock: Lock, text: String)(using Logger[IO]): IO[Boolean] =
    for
      authenticatedEndpoints <- user.authenticatedEndpoints(client)
      _ <- authenticatedEndpoints.updateSettings(lock._id, _.copy(hideTimeLogs = true))
    yield true

  override val description: String = "Hides times from the lock log"