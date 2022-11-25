package lurch.wheel

import bot.chaster.{ConfigUpdate, DiceConfig, LinkConfig}
import bot.model.{ChasterID, RegisteredUser}
import bot.chaster.Client.{*, given}
import bot.syntax.io.*
import bot.tasks.ModifierTextWheelCommand.Modifier
import bot.tasks.ModifierTextWheelCommand
import cats.data.OptionT
import cats.effect.IO
import cats.syntax.functor.*
import lurch.Lurch
import org.typelevel.log4cats.Logger
import scala.reflect.Typeable

import scala.concurrent.duration.*

object DiceMultiplier extends ModifierTextWheelCommand[DiceConfig]:
  override def textPattern: String = "DiceMultiplier:"
  override def logName: String = "dice multiplier"

  override def configUpdate(configUpdate: ConfigUpdate[DiceConfig], modifier: Modifier)(using Typeable[DiceConfig]): ConfigUpdate[DiceConfig] =
    configUpdate.copy(
      config = configUpdate.config.copy(
        multiplier = modifier.apply(configUpdate.config.multiplier.toMinutes.toInt).minutes
      )
    )
