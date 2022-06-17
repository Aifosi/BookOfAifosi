package bookofaifosi

import com.typesafe.config.{Config, ConfigFactory}
import pureconfig.{ConfigReader, ConfigSource}
import pureconfig.generic.derivation.default.derived

case class Configuration(
  token: String,
) derives ConfigReader

object Configuration:

  def fromConfig(config: Config = ConfigFactory.load()): Configuration =
    ConfigSource.fromConfig(config).loadOrThrow[Configuration]
