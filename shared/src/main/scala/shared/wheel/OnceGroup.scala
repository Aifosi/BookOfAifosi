package shared.wheel

import bot.Bot
import bot.chaster.Client.{*, given}
import bot.chaster.{Lock, Segment, WheelOfFortuneConfig}
import bot.db.RegisteredUserRepository
import bot.model.{ChasterID, RegisteredUser}
import bot.syntax.io.*
import bot.tasks.{WheelCommand, keyholder}
import cats.data.OptionT
import cats.effect.IO
import org.typelevel.log4cats.Logger

import scala.annotation.tailrec

object OnceGroup extends WheelCommand:
  private val onceGroupRegex = "OnceGroup (.+?): (.+)".r

  override def apply(user: RegisteredUser, lock: Lock, segment: Segment)(using Logger[IO]): IO[(Boolean, Segment)] =
    val originalText = segment.text
    originalText match
      case onceGroupRegex(group, text) =>
        (for
          keyholder <- keyholder(lock)
          regex = s"OnceGroup $group:".r
          _ <- OptionT.liftF {
            keyholder.updateExtension[WheelOfFortuneConfig](lock._id) { configUpdate =>
              configUpdate.copy(
                config = configUpdate.config.copy(
                  segments = configUpdate.config.segments.filter(segment => regex.findFirstIn(segment.text).isEmpty)
                )
              )
            }
          }
          _ <- OptionT.liftF(Logger[IO].debug(s"Removed all OnceGroup options from the wheel of $user"))
          _ <- Bot.channels.spinlog.sendMessage(s"Removed all OnceGroup options from the wheel of ${user.mention}")
        yield ()).value.as((false, segment.copy(text = text)))
      case _ => IO.pure((false, segment))
