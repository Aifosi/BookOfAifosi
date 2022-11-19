package lurch.wheel

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

object Unique extends WheelCommand:
  private val uniqueRegex = "Unique: (.+)".r

  override def apply(user: RegisteredUser, lockID: ChasterID, segment: Segment)(using Logger[IO]): IO[(Boolean, Segment)] =
    val originalText = segment.text
    originalText match
      case uniqueRegex(text) =>
        (for
        (_, keyholder) <- lockAndKeyholder(user, lockID)
        _ <- OptionT.liftF {
        keyholder.updateExtension[WheelOfFortuneConfig](lockID) { configUpdate =>
          configUpdate.copy(
            config = configUpdate.config.copy(
              segments = configUpdate.config.segments.filter(segment => !uniqueRegex.matches(segment.text))
            )
          )
        }
      }
        _ <- OptionT.liftF(Logger[IO].debug(s"Removed all unique options from the wheel of $user"))
        _ <- Lurch.channels.spinlog.sendMessage(s"Removed all unique options from the wheel of ${user.mention}")
        yield ()).value.as((false, segment.copy(text = text)))
      case _ => IO.pure((false, segment))
