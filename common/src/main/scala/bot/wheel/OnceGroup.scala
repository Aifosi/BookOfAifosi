package bot.wheel

import bot.{Bot, DiscordLogger}
import bot.chaster.{ChasterClient, Lock, Segment, WheelOfFortuneConfig}
import bot.db.RegisteredUserRepository
import bot.model.{ChasterID, RegisteredUser}
import bot.syntax.io.*
import bot.tasks.WheelCommand
import cats.data.OptionT
import cats.effect.IO
import org.typelevel.log4cats.Logger

import scala.annotation.tailrec
import scala.util.matching.Regex

class OnceGroup(
  client: ChasterClient,
  registeredUserRepository: RegisteredUserRepository,
)(using discordLogger: DiscordLogger) extends WheelCommand(client, registeredUserRepository):
  private val onceGroupRegex = "OnceGroup (.+?): (.+)".r

  override def apply(user: RegisteredUser, lock: Lock, segment: Segment)(using Logger[IO]): IO[(Boolean, Segment)] =
    segment.text match
      case onceGroupRegex(group, text) =>
        authenticatedEndpoints(lock).semiflatMap { authenticatedEndpoints =>
          for
            _ <- authenticatedEndpoints.updateExtension[WheelOfFortuneConfig](lock._id) { configUpdate =>
              configUpdate.copy(
                config = configUpdate.config.copy(
                  segments = configUpdate.config.segments.filter(segment => s"OnceGroup $group:".r.findFirstIn(segment.text).isEmpty)
                )
              )
            }
            _ <- Logger[IO].debug(s"Removed all OnceGroup options from the wheel of $user")
            _ <- discordLogger.logToSpinlog(s"Removed all OnceGroup options from the wheel of ${user.mention}")
          yield ()
        }
          .value
          .as((false, segment.copy(text = text)))
      case _ => IO.pure((false, segment))
