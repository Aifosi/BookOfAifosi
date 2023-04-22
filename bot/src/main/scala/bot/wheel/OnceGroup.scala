package bot.wheel

import bot.{Bot, DiscordLogger}
import bot.chaster.ChasterClient
import bot.chaster.model.{Lock, Segment, WheelOfFortuneConfig}
import bot.db.RegisteredUserRepository
import bot.model.{ChasterID, RegisteredUser}
import bot.syntax.io.*
import bot.tasks.WheelCommand
import bot.syntax.kleisli.*
import bot.instances.functionk.given

import cats.data.OptionT
import cats.effect.IO
import org.typelevel.log4cats.Logger
import scala.annotation.tailrec
import scala.util.matching.Regex

class OnceGroup(
  client: ChasterClient,
  registeredUserRepository: RegisteredUserRepository,
)(using discordLogger: DiscordLogger)
    extends WheelCommand[Regex](client, registeredUserRepository):
  override val pattern: Regex = "OnceGroup (.+?): (.+)".r

  override def apply(user: RegisteredUser, lock: Lock, segment: Segment)(using Logger[IO]): IO[(Boolean, Segment)] =
    segment.text match
      case pattern(group, text) =>
        keyholder(lock, user.guildID).semiflatMap { keyholder =>
          for
            _ <- client
              .updateExtension[WheelOfFortuneConfig](lock._id) { configUpdate =>
                configUpdate.copy(
                  config = configUpdate.config.copy(
                    segments = configUpdate.config.segments.filter(segment =>
                      s"OnceGroup $group:".r.findFirstIn(segment.text).isEmpty,
                    ),
                  ),
                )
              }
              .runUsingTokenOf(keyholder)
            _ <- Logger[IO].debug(s"Removed all OnceGroup options from the wheel of $user")
            _ <- discordLogger.logToSpinlog(s"Removed all OnceGroup options from the wheel of ${user.mention}")
          yield ()
        }.value
          .as((false, segment.copy(text = text)))
      case _                    => IO.pure((false, segment))

  override val description: String =
    "A segment that deletes itself and all others in the same group after being rolled (wheels must always have at least two segments after deleting)"
