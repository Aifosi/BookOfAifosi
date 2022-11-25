package lurch.wheel

import bot.chaster.{ConfigUpdate, PenaltyConfig, VerificationPictureVerifyPunishmentConfig}
import bot.tasks.ModifierTextWheelCommand
import bot.tasks.ModifierTextWheelCommand.Modifier
import scala.reflect.Typeable

object VerificationPictures extends ModifierTextWheelCommand[PenaltyConfig]:
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
