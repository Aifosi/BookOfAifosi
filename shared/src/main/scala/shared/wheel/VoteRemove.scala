package shared.wheel

import bot.chaster.Client.{*, given}
import bot.chaster.{ConfigUpdate, LinkConfig}
import bot.model.{ChasterID, RegisteredUser}
import bot.syntax.io.*
import bot.tasks.ModifierTextWheelCommand
import bot.tasks.ModifierTextWheelCommand.Modifier
import bot.tasks.ModifierTextWheelCommand.Modifier.*
import cats.data.OptionT
import cats.effect.IO
import org.typelevel.log4cats.Logger

import scala.concurrent.duration.*
import scala.reflect.Typeable

object VoteRemove extends ModifierTextWheelCommand[LinkConfig]:
  override def textPattern: String = "VoteRemove:"
  override def logName: String = "time removed per vote"

  override def configUpdate(configUpdate: ConfigUpdate[LinkConfig], modifier: Modifier)(using Typeable[LinkConfig]): ConfigUpdate[LinkConfig] =
    configUpdate.copy(
      config = configUpdate.config.copy(
        timeToRemove = modifier.apply(configUpdate.config.timeToRemove.toMinutes.toInt).minutes
      )
    )
