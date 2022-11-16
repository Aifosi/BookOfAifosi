package lurch.wheel

import bot.chaster.LinkConfig
import bot.model.{ChasterID, RegisteredUser}
import bot.chaster.Client.{*, given}
import bot.syntax.io.*
import bot.tasks.ModifierTextWheelCommand
import bot.tasks.ModifierTextWheelCommand.Modifier
import bot.tasks.ModifierTextWheelCommand.Modifier.*
import cats.data.OptionT
import cats.effect.IO
import lurch.Lurch
import org.typelevel.log4cats.Logger

object VoteTarget extends ModifierTextWheelCommand:
  override def textPattern: String = "VoteTarget:"

  override def run(user: RegisteredUser, lockID: ChasterID, modifier: Modifier)(using Logger[IO]): IO[Boolean] =
    (for
      (_, keyholder) <- lockAndKeyholder(user, lockID)
      _ <- OptionT.liftF(keyholder.updateExtension[LinkConfig](lockID) { configUpdate =>
        val updatedVisits = modifier.apply(configUpdate.config.nbVisits)
        configUpdate.copy(config = configUpdate.config.copy(nbVisits = updatedVisits))
      })
      message = modifier match {
        case Add(value) => s"by +$value"
        case Remove(value) => s"by -$value"
        case Exact(value) => s"to $value"
      }
      _ <- OptionT.liftF(Logger[IO].debug(s"$user required votes changed $message"))
      _ <- Lurch.channels.spinlog.sendMessage(s"${user.mention} required votes changed $message")
    yield ())
      .fold(false)(_ => true)
