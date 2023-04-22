package bot

import bot.model.{Channel, DiscordID}

import cats.data.OptionT
import cats.effect.IO
import com.typesafe.config.{Config, ConfigFactory}
import pureconfig.{ConfigReader, ConfigSource}
import pureconfig.generic.derivation.default.derived
import pureconfig.module.catseffect.syntax.*

case class LurchChannelConfiguration(
  tortureChamberID: Option[DiscordID],
) derives ConfigReader

object LurchChannelConfiguration:
  def fromConfig(config: Config = ConfigFactory.load()): IO[LurchChannelConfiguration] =
    ConfigSource.fromConfig(config).at("channels").loadF()
