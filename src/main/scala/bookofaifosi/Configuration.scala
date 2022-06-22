package bookofaifosi

import com.typesafe.config.{Config, ConfigFactory}
import pureconfig.{ConfigReader, ConfigSource}
import pureconfig.generic.derivation.default.derived

import scala.concurrent.duration.FiniteDuration

case class Configuration(
  host: String,
  publicHost: String,
  port: Int,
  checkFrequency: FiniteDuration,
) derives ConfigReader

object Configuration:

  def fromConfig(config: Config = ConfigFactory.load()): Configuration =
    ConfigSource.fromConfig(config).at("app").loadOrThrow[Configuration]
