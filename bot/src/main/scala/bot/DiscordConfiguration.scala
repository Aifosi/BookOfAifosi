package bot

import com.typesafe.config.{Config, ConfigFactory}
import pureconfig.generic.derivation.default.derived
import pureconfig.{ConfigReader, ConfigSource}

case class DiscordConfiguration(
  token: String,
) derives ConfigReader

object DiscordConfiguration:
  def fromConfig(config: Config = ConfigFactory.load()): DiscordConfiguration =
    ConfigSource.fromConfig(config).at("discord").loadOrThrow[DiscordConfiguration]
