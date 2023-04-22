package bot.chaster.model

import bot.chaster.model.instances.*

import io.circe.{Decoder, Encoder}
import io.circe.syntax.*

sealed trait PunishmentConfig

object PunishmentConfig:
  given Encoder[PunishmentConfig] = punishmentConfig =>
    val punishmentConfigEncoder: Encoder[PunishmentConfig] = Encoder.instance {
      case punishment: WheelOfFortuneTurnsPunishmentConfig       => punishment.asJson
      case punishment: DiceRollPunishmentConfig                  => punishment.asJson
      case punishment: TasksPunishmentConfig                     => punishment.asJson
      case punishment: TasksDoTaskPunishmentConfig               => punishment.asJson
      case punishment: TemporaryOpeningOpenPunishmentConfig      => punishment.asJson
      case punishment: TemporaryOpeningTimeLimitPunishmentConfig => punishment.asJson
      case punishment: VerificationPictureVerifyPunishmentConfig => punishment.asJson
    }
    val name                                               =
      punishmentConfig.getClass.getSimpleName.split("\\$").last.replace("PunishmentConfig", "").pascalToSnakeCase
    punishmentConfigEncoder
      .apply(punishmentConfig)
      .mapObject(_.add("name", name.asJson).add("prefix", "default".asJson))

  given Decoder[PunishmentConfig] = cursor =>
    lazy val frequencyParam = cursor.get[FrequencyParam]("params")
    lazy val timeLimitParam = cursor.get[TimeLimitParam]("params")
    cursor.get[List[Punishment]]("punishments").flatMap { punishments =>
      cursor.get[String]("name").flatMap {
        case "wheel_of_fortune_turns"       => frequencyParam.map(WheelOfFortuneTurnsPunishmentConfig.apply(_, punishments))
        case "dice_roll"                    => frequencyParam.map(DiceRollPunishmentConfig.apply(_, punishments))
        case "tasks"                        => frequencyParam.map(TasksPunishmentConfig.apply(_, punishments))
        case "tasks_do_task"                => timeLimitParam.map(TasksDoTaskPunishmentConfig.apply(_, punishments))
        case "temporary_opening_open"       => frequencyParam.map(TemporaryOpeningOpenPunishmentConfig.apply(_, punishments))
        case "temporary_opening_time_limit" =>
          timeLimitParam.map(TemporaryOpeningTimeLimitPunishmentConfig.apply(_, punishments))
        case "verification_picture_verify"  =>
          frequencyParam.map(VerificationPictureVerifyPunishmentConfig.apply(_, punishments))
      }
    }

case class WheelOfFortuneTurnsPunishmentConfig(
  params: FrequencyParam,
  punishments: List[Punishment],
) extends PunishmentConfig
    derives Decoder, Encoder.AsObject

case class DiceRollPunishmentConfig(
  params: FrequencyParam,
  punishments: List[Punishment],
) extends PunishmentConfig
    derives Decoder, Encoder.AsObject

case class TasksPunishmentConfig(
  params: FrequencyParam,
  punishments: List[Punishment],
) extends PunishmentConfig
    derives Decoder, Encoder.AsObject

case class TasksDoTaskPunishmentConfig(
  params: TimeLimitParam,
  punishments: List[Punishment],
) extends PunishmentConfig
    derives Decoder, Encoder.AsObject

case class TemporaryOpeningOpenPunishmentConfig(
  params: FrequencyParam,
  punishments: List[Punishment],
) extends PunishmentConfig
    derives Decoder, Encoder.AsObject

case class TemporaryOpeningTimeLimitPunishmentConfig(
  params: TimeLimitParam,
  punishments: List[Punishment],
) extends PunishmentConfig
    derives Decoder, Encoder.AsObject

case class VerificationPictureVerifyPunishmentConfig(
  params: FrequencyParam,
  punishments: List[Punishment],
) extends PunishmentConfig
    derives Decoder, Encoder.AsObject
