package lurch.wheel

import bot.Bot
import bot.chaster.{Segment, WheelOfFortuneConfig}
import bot.chaster.Client.{*, given}
import bot.model.{ChasterID, RegisteredUser}
import bot.syntax.io.*
import bot.tasks.WheelCommand
import cats.data.OptionT
import cats.effect.IO
import lurch.Lurch
import org.typelevel.log4cats.Logger

import scala.annotation.tailrec

object Once extends WheelCommand:
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

  override def apply(user: RegisteredUser, lockID: ChasterID, segment: Segment)(using Logger[IO]): IO[(Boolean, Segment)] =
    val originalText = segment.text
    originalText match
      case onceRegex(text) =>
        (for
          (_, keyholder) <- lockAndKeyholder(user, lockID)
          _ <- OptionT.liftF {
            keyholder.updateExtension[WheelOfFortuneConfig](lockID) { configUpdate =>
              configUpdate.copy(
                config = configUpdate.config.copy(
                  segments = dropFirstSegment(configUpdate.config.segments, originalText)
                ),
              )
            }
          }
          _ <- OptionT.liftF(Logger[IO].debug(s"Removed option $text from the wheel of $user"))
          _ <- Bot.channels.spinlog.sendMessage(s"Removed option $text from the wheel of ${user.mention}")
        yield ()).value.as((false, segment.copy(text = text)))
      case _ => IO.pure((false, segment))
