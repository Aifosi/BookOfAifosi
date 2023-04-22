package bot.wheel

import bot.DiscordLogger
import bot.chaster.ChasterClient
import bot.chaster.model.Lock
import bot.db.RegisteredUserRepository
import bot.instances.functionk.given
import bot.model.RegisteredUser
import bot.syntax.kleisli.*
import bot.tasks.TextWheelCommand

import cats.effect.IO
import org.typelevel.log4cats.Logger
import scala.util.matching.Regex

class LogTimesShow(
  client: ChasterClient,
  registeredUserRepository: RegisteredUserRepository,
)(using discordLogger: DiscordLogger)
    extends TextWheelCommand(client, registeredUserRepository):
  override lazy val pattern: Regex                                                                = "ShowLogTimes".r
  override def run(user: RegisteredUser, lock: Lock, text: String)(using Logger[IO]): IO[Boolean] =
    client.updateSettings(lock._id, _.copy(hideTimeLogs = false)).runUsingTokenOf(user).as(true)

  override val description: String = "Shows times in the lock log"
