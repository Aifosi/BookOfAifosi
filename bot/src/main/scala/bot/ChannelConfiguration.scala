package bot

import bot.model.DiscordID

import cats.effect.IO
import com.typesafe.config.{Config, ConfigFactory}
import pureconfig.{ConfigReader, ConfigSource}
import pureconfig.generic.derivation.default.derived
import pureconfig.module.catseffect.syntax.*

case class ChannelConfiguration(
  logID: Option[DiscordID],
  spinlogID: Option[DiscordID],
) derives ConfigReader

object ChannelConfiguration:
  def fromConfig(config: Config = ConfigFactory.load()): IO[ChannelConfiguration] =
    ConfigSource.fromConfig(config).at("channels").loadF()
