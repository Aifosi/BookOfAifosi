package bot.wheel

import bot.DiscordLogger
import bot.chaster.ChasterClient
import bot.chaster.model.{ConfigUpdate, DiceRollPunishmentConfig, PenaltyConfig, WheelOfFortuneTurnsPunishmentConfig}
import bot.db.RegisteredUserRepository
import bot.tasks.ModifierTextWheelCommand
import bot.tasks.ModifierTextWheelCommand.Modifier

import scala.reflect.Typeable

class DiceRolls(
  client: ChasterClient,
  registeredUserRepository: RegisteredUserRepository,
)(using DiscordLogger)
    extends ModifierTextWheelCommand[PenaltyConfig](client, registeredUserRepository):
  override def textPattern: String = "DiceRolls:"

  override def logName: String = "dice rolls"

  override def configUpdate(configUpdate: ConfigUpdate[PenaltyConfig], modifier: Modifier)(using
    Typeable[PenaltyConfig],
  ): ConfigUpdate[PenaltyConfig] =
    configUpdate.copy(
      config = configUpdate.config.copy(
        penalties = configUpdate.config.penalties.map {
          case config: DiceRollPunishmentConfig =>
            config.copy(params = config.params.copy(nbActions = modifier.apply(config.params.nbActions)))
          case config                           => config
        },
      ),
    )
  override val description: String = "Changes the number of required dice rolls"
