package lurch.wheel

import bot.chaster.{ConfigUpdate, ExtensionConfig, PenaltyConfig, VerificationPictureVerifyPunishmentConfig}
import bot.syntax.io.*
import bot.tasks.ModifierTextWheelCommand
import bot.tasks.ModifierTextWheelCommand.Modifier
import cats.data.OptionT
import cats.effect.IO
import cats.syntax.functor.*
import lurch.Lurch

import scala.reflect.Typeable

abstract class ModifierWheelCommand[Config <: ExtensionConfig: Typeable] extends ModifierTextWheelCommand[Config]:
  override def channelLog(message: String): OptionT[IO, Unit] = Lurch.channels.spinlog.sendMessage(message).void