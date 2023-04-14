package bot.wheel

import bot.DiscordLogger
import bot.chaster.{ChasterClient, ConfigUpdate, PenaltyConfig, WheelOfFortuneTurnsPunishmentConfig}
import bot.db.RegisteredUserRepository
import bot.tasks.ModifierTextWheelCommand
import bot.tasks.ModifierTextWheelCommand.Modifier

import scala.reflect.Typeable

class WheelRolls(
  client: ChasterClient,
  registeredUserRepository: RegisteredUserRepository,
)(using DiscordLogger) extends ModifierTextWheelCommand[PenaltyConfig](client, registeredUserRepository):
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
