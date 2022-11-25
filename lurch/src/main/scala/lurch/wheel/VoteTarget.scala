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

object VoteTarget extends ModifierTextWheelCommand[LinkConfig]:
  override def textPattern: String = "VoteTarget:"
  override def logName: String = "required votes"

  override def configUpdate(configUpdate: ConfigUpdate[LinkConfig], modifier: Modifier)(using Typeable[LinkConfig]): ConfigUpdate[LinkConfig] =
    configUpdate.copy(
      config = configUpdate.config.copy(
        nbVisits = modifier.apply(configUpdate.config.nbVisits)
      )
    )
