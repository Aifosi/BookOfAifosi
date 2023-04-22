package bot

import bot.ChannelConfiguration

import cats.effect.IO
import com.typesafe.config.{Config, ConfigFactory}
import java.time.Instant
import pureconfig.{ConfigReader, ConfigSource}
import pureconfig.generic.derivation.default.derived
import pureconfig.module.catseffect.syntax.*
import scala.concurrent.duration.FiniteDuration

case class Configuration(
  checkFrequency: FiniteDuration,
) derives ConfigReader

object Configuration:
  def fromConfig(config: Config = ConfigFactory.load()): IO[Configuration] =
    ConfigSource.fromConfig(config).at("book-of-aifosi").loadF()
