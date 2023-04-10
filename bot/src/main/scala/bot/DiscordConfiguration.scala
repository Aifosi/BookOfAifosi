package bot

import cats.effect.IO
import com.typesafe.config.{Config, ConfigFactory}
import pureconfig.generic.derivation.default.derived
import pureconfig.module.catseffect.syntax.*
import pureconfig.{ConfigReader, ConfigSource}

case class DiscordConfiguration(
  token: String,
) derives ConfigReader

object DiscordConfiguration:
  def fromConfig(config: Config = ConfigFactory.load()): IO[DiscordConfiguration] =
    ConfigSource.fromConfig(config).at("discord").loadF()
