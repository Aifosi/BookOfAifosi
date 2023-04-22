package bot.chaster.model

import bot.chaster.model.instances.{*, given}

import io.circe.{Decoder, Encoder}
import io.circe.syntax.*
import scala.concurrent.duration.FiniteDuration

sealed trait Punishment

case class AddTimePunishment(params: FiniteDuration) extends Punishment derives Decoder, Encoder.AsObject

case object FreezePunishment extends Punishment derives Decoder, Encoder.AsObject

case class PilloryPunishment(params: DurationParam) extends Punishment derives Decoder, Encoder.AsObject

object Punishment:
  given Encoder[Punishment] = punishment =>
    val partialEncoder: Encoder[Punishment] = Encoder.instance {
      case punishment: AddTimePunishment     => punishment.asJson
      case punishment: FreezePunishment.type => punishment.asJson
      case punishment: PilloryPunishment     => punishment.asJson
    }
    val name                                = punishment.getClass.getSimpleName.split("\\$").last.replace("Punishment", "").pascalToSnakeCase
    partialEncoder.apply(punishment).mapObject(_.add("name", name.asJson))

  given Decoder[Punishment] = cursor =>
    cursor.get[String]("name").flatMap {
      case "add_time" => cursor.get[FiniteDuration]("params").map(AddTimePunishment.apply)
      case "freeze"   => Right(FreezePunishment)
      case "pillory"  => cursor.get[DurationParam]("params").map(PilloryPunishment.apply)
    }
