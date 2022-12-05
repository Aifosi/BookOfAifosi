package shared.wheel

import bot.chaster.{ConfigUpdate, DiceRollPunishmentConfig, PenaltyConfig, WheelOfFortuneTurnsPunishmentConfig}
import bot.tasks.ModifierTextWheelCommand
import bot.tasks.ModifierTextWheelCommand.Modifier

import scala.reflect.Typeable

object DiceRolls extends ModifierTextWheelCommand[PenaltyConfig]:
  override def textPattern: String = "DiceRolls:"

  override def logName: String = "dice rolls"

  override def configUpdate(configUpdate: ConfigUpdate[PenaltyConfig], modifier: Modifier)(using Typeable[PenaltyConfig]): ConfigUpdate[PenaltyConfig] =
    configUpdate.copy(
      config = configUpdate.config.copy(
        penalties = configUpdate.config.penalties.map {
          case config: DiceRollPunishmentConfig =>
            config.copy(params = config.params.copy(nbActions = modifier.apply(config.params.nbActions)))
          case config => config
        }
      )
    )
