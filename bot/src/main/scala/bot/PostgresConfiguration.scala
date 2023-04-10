package bot

import cats.effect.IO
import com.typesafe.config.{Config, ConfigFactory}
import pureconfig.*
import pureconfig.generic.derivation.default.derived
import pureconfig.module.catseffect.syntax.*

final case class PostgresConfiguration(
  driver: String,
  user: String,
  password: String,
  url: String
) extends DBConfig derives ConfigReader

object PostgresConfiguration:
  def fromConfig(config: Config = ConfigFactory.load()): IO[PostgresConfiguration] =
    ConfigSource.fromConfig(config).at("postgres").loadF()
