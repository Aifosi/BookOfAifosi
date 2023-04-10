package bot

import cats.effect.IO
import com.typesafe.config.{Config, ConfigFactory}
import pureconfig.*
import pureconfig.generic.derivation.default.derived
import pureconfig.module.catseffect.syntax.*

final case class MysqlConfiguration(
  driver: String,
  user: String,
  password: String,
  url: String
) extends DBConfig derives ConfigReader

object MysqlConfiguration:
  def fromConfig(config: Config = ConfigFactory.load()): IO[MysqlConfiguration] =
    ConfigSource.fromConfig(config).at("mysql").loadF()
