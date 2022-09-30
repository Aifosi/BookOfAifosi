package bookofaifosi

import com.typesafe.config.{Config, ConfigFactory}
import pureconfig.*
import pureconfig.generic.derivation.default.derived

final case class PostgresConfiguration(
  driver: String,
  user: String,
  password: String,
  url: String
) derives ConfigReader

object PostgresConfiguration:
  def fromConfig(config: Config = ConfigFactory.load()): PostgresConfiguration = ConfigSource.fromConfig(config).at("postgres").loadOrThrow[PostgresConfiguration]
