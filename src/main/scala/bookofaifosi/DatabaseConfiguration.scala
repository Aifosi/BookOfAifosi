package bookofaifosi

import com.typesafe.config.{Config, ConfigFactory}
import pureconfig.*
import pureconfig.generic.derivation.default.derived

final case class DatabaseConfiguration(
  driver: String,
  user: String,
  password: String,
  url: String
) derives ConfigReader

object DatabaseConfiguration:
  def fromConfig(config: Config = ConfigFactory.load()): DatabaseConfiguration = ConfigSource.fromConfig(config).at("db").loadOrThrow[DatabaseConfiguration]
