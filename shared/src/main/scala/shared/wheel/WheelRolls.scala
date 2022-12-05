package shared.wheel

import bot.chaster.{ConfigUpdate, PenaltyConfig, WheelOfFortuneTurnsPunishmentConfig}
import bot.tasks.ModifierTextWheelCommand
import bot.tasks.ModifierTextWheelCommand.Modifier

import scala.reflect.Typeable

object WheelRolls extends ModifierTextWheelCommand[PenaltyConfig]:
  override def textPattern: String = "WheelRolls:"

  override def logName: String = "wheel of fortune rolls"

  override def configUpdate(configUpdate: ConfigUpdate[PenaltyConfig], modifier: Modifier)(using Typeable[PenaltyConfig]): ConfigUpdate[PenaltyConfig] =
    configUpdate.copy(
      config = configUpdate.config.copy(
        penalties = configUpdate.config.penalties.map {
          case config: WheelOfFortuneTurnsPunishmentConfig =>
            config.copy(params = config.params.copy(nbActions = modifier.apply(config.params.nbActions)))
          case config => config
        }
      )
    )
