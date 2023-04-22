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

class Once(
  client: ChasterClient,
  registeredUserRepository: RegisteredUserRepository,
)(using discordLogger: DiscordLogger)
    extends WheelCommand[Regex](client, registeredUserRepository):
  override val pattern: Regex = "Once: (.+)".r

  @tailrec private def dropFirstSegment(
    segments: List[Segment],
    text: String,
    doneSegments: List[Segment] = List.empty,
  ): List[Segment] =
    segments match
      case Nil                               => doneSegments
      case head :: tail if head.text == text => doneSegments ++ tail
      case head :: tail                      => dropFirstSegment(tail, text, doneSegments :+ head)

  override def apply(user: RegisteredUser, lock: Lock, segment: Segment)(using Logger[IO]): IO[(Boolean, Segment)] =
    val originalText = segment.text
    originalText match
      case pattern(text) =>
        keyholder(lock, user.guildID).semiflatMap { keyholder =>
          for
            _ <- client
              .updateExtension[WheelOfFortuneConfig](lock._id) { configUpdate =>
                configUpdate.copy(
                  config = configUpdate.config.copy(
                    segments = dropFirstSegment(configUpdate.config.segments, originalText),
                  ),
                )
              }
              .runUsingTokenOf(keyholder)
            _ <- Logger[IO].debug(s"Removed option $text from the wheel of $user")
            _ <- discordLogger.logToSpinlog(s"Removed option $text from the wheel of ${user.mention}")
          yield ()
        }.value
          .as((false, segment.copy(text = text)))
      case _             => IO.pure((false, segment))

  override val description: String =
    "A segment that deletes itself after being rolled (wheels must always have at least two segments after deleting)"
