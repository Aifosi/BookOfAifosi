package shared.wheel

import bot.chaster.{ConfigUpdate, PilloryConfig}
import bot.tasks.ModifierTextWheelCommand

import scala.concurrent.duration.*
import scala.reflect.Typeable

object PilloryVoteTime extends ModifierTextWheelCommand[PilloryConfig]:
  override def textPattern: String = "PilloryVoteTime:"

  override def logName: String = "pillory vote time"

  override def configUpdate(configUpdate: ConfigUpdate[PilloryConfig], modifier: ModifierTextWheelCommand.Modifier)(using Typeable[PilloryConfig]): ConfigUpdate[PilloryConfig] =
    configUpdate.copy(
      config = configUpdate.config.copy(
        timeToAdd = modifier.apply(configUpdate.config.timeToAdd.toMinutes.toInt).minutes
      )
    )

