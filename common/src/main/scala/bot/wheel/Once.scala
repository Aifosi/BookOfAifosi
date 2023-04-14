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

class Once(
  client: ChasterClient,
  registeredUserRepository: RegisteredUserRepository,
)(using discordLogger: DiscordLogger) extends WheelCommand(client, registeredUserRepository):
  private val onceRegex = "Once: (.+)".r

  @tailrec private def dropFirstSegment(
    segments: List[Segment],
    text: String,
    doneSegments: List[Segment] = List.empty,
  ): List[Segment] =
    segments match
      case Nil => doneSegments
      case head :: tail if head.text == text => doneSegments ++ tail
      case head :: tail => dropFirstSegment(tail, text, doneSegments :+ head)

  override def apply(user: RegisteredUser, lock: Lock, segment: Segment)(using Logger[IO]): IO[(Boolean, Segment)] =
    val originalText = segment.text
    originalText match
      case onceRegex(text) =>
        authenticatedEndpoints(lock).semiflatMap { authenticatedEndpoints =>
          for
            _ <- authenticatedEndpoints.updateExtension[WheelOfFortuneConfig](lock._id) { configUpdate =>
              configUpdate.copy(
                config = configUpdate.config.copy(
                  segments = dropFirstSegment(configUpdate.config.segments, originalText)
                ),
              )
            }
            _ <- Logger[IO].debug(s"Removed option $text from the wheel of $user")
            _ <- discordLogger.logToSpinlog(s"Removed option $text from the wheel of ${user.mention}")
          yield ()
        }
          .value
          .as((false, segment.copy(text = text)))
      case _ => IO.pure((false, segment))
