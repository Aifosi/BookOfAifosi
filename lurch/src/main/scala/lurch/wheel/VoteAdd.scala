package lurch.wheel

import bot.chaster.{ConfigUpdate, LinkConfig}
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
import scala.reflect.Typeable
import scala.concurrent.duration.*

object VoteAdd extends ModifierWheelCommand[LinkConfig]:
  override def textPattern: String = "VoteAdd:"
  override def logName: String = "time added per vote"

  override def configUpdate(configUpdate: ConfigUpdate[LinkConfig], modifier: Modifier)(using Typeable[LinkConfig]): ConfigUpdate[LinkConfig] =
    configUpdate.copy(
      config = configUpdate.config.copy(
        timeToAdd = modifier.apply(configUpdate.config.timeToAdd.toMinutes.toInt).minutes
      )
    )
    