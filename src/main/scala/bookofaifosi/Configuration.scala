package bookofaifosi

import com.typesafe.config.{Config, ConfigFactory}
import pureconfig.{ConfigReader, ConfigSource}
import pureconfig.generic.derivation.default.derived

import scala.concurrent.duration.FiniteDuration

case class PilloryBitches(
    hours: Int,
    minutes: Int,
) derives ConfigReader

case class Configuration(
  host: String,
  port: Int,
  publicHost: String,
  publicPort: Int,
  checkFrequency: FiniteDuration,
  pilloryBitches: PilloryBitches,
) derives ConfigReader

object Configuration:

  def fromConfig(config: Config = ConfigFactory.load()): Configuration =
    ConfigSource.fromConfig(config).at("app").loadOrThrow[Configuration]
