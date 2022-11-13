package bot

import com.typesafe.config.{Config, ConfigFactory}
import pureconfig.*
import pureconfig.generic.derivation.default.derived

final case class MysqlConfiguration(
  driver: String,
  user: String,
  password: String,
  url: String
) extends DBConfig derives ConfigReader

object MysqlConfiguration:
  def fromConfig(config: Config = ConfigFactory.load()): MysqlConfiguration = ConfigSource.fromConfig(config).at("mysql").loadOrThrow[MysqlConfiguration]
