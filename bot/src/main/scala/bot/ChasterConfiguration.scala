package bot

import com.typesafe.config.{Config, ConfigFactory}
import pureconfig.generic.derivation.default.derived
import pureconfig.{ConfigReader, ConfigSource}

case class ChasterConfiguration(
  host: String,
  port: Int,
  publicHost: String,
  publicPort: Int,
  clientId: String,
  secretKey: String,
) derives ConfigReader

object ChasterConfiguration:
  def fromConfig(config: Config = ConfigFactory.load()): ChasterConfiguration =
    ConfigSource.fromConfig(config).at("chaster").loadOrThrow[ChasterConfiguration]
