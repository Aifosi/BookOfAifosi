package bot.chaster.model

import bot.chaster.model.instances.{*, given}

import io.circe.{Encoder, Json}
import io.circe.syntax.*
import scala.concurrent.duration.FiniteDuration

case class ConfigUpdate[+Config <: ExtensionConfig](
  config: Config,
  mode: AvailableModes,
  regularity: FiniteDuration,
)

object ConfigUpdate:
  given Encoder[ConfigUpdate[ExtensionConfig]] = configUpdate =>
    Json.obj(
      "slug"       -> configUpdate.config.getClass.getSimpleName.replace("Config", "").pascalToKebabCase.asJson,
      "config"     -> configUpdate.config.asJson,
      "mode"       -> configUpdate.mode.asJson,
      "regularity" -> configUpdate.regularity.asJson,
    )

case class ConfigUpdatePayload(
  extensions: List[ConfigUpdate[ExtensionConfig]],
) derives Encoder.AsObject
