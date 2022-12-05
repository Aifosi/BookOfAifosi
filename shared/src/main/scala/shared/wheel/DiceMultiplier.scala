package shared.wheel

import bot.chaster.Client.{*, given}
import bot.chaster.{ConfigUpdate, DiceConfig, LinkConfig}
import bot.model.{ChasterID, RegisteredUser}
import bot.syntax.io.*
import bot.tasks.ModifierTextWheelCommand
import bot.tasks.ModifierTextWheelCommand.Modifier
import cats.data.OptionT
import cats.effect.IO
import cats.syntax.functor.*
import org.typelevel.log4cats.Logger

import scala.concurrent.duration.*
import scala.reflect.Typeable

object DiceMultiplier extends ModifierTextWheelCommand[DiceConfig]:
  override def textPattern: String = "DiceMultiplier:"
  override def logName: String = "dice multiplier"

  override def configUpdate(configUpdate: ConfigUpdate[DiceConfig], modifier: Modifier)(using Typeable[DiceConfig]): ConfigUpdate[DiceConfig] =
    configUpdate.copy(
      config = configUpdate.config.copy(
        multiplier = modifier.apply(configUpdate.config.multiplier.toMinutes.toInt).minutes
      )
    )
