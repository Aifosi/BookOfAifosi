package shared.wheel

import bot.DiscordLogger
import bot.chaster.{ChasterClient, ConfigUpdate, PenaltyConfig, VerificationPictureVerifyPunishmentConfig}
import bot.db.RegisteredUserRepository
import bot.tasks.ModifierTextWheelCommand
import bot.tasks.ModifierTextWheelCommand.Modifier

import scala.reflect.Typeable

class VerificationPictures(
  client: ChasterClient,
  registeredUserRepository: RegisteredUserRepository,
)(using DiscordLogger) extends ModifierTextWheelCommand[PenaltyConfig](client, registeredUserRepository):
  override def textPattern: String = "VerificationPictures:"

  override def logName: String = "verification pictures"

  override def configUpdate(configUpdate: ConfigUpdate[PenaltyConfig], modifier: Modifier)(using Typeable[PenaltyConfig]): ConfigUpdate[PenaltyConfig] =
    configUpdate.copy(
      config = configUpdate.config.copy(
        penalties = configUpdate.config.penalties.map {
          case config: VerificationPictureVerifyPunishmentConfig =>
            config.copy(params = config.params.copy(nbActions = modifier.apply(config.params.nbActions)))
          case config => config
        }
      )
    )
