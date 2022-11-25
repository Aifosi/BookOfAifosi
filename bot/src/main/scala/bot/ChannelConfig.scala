package bot

import bot.Utils.getChannel
import bot.model.{Channel, DiscordID}
import cats.data.OptionT
import cats.effect.IO
import com.typesafe.config.{Config, ConfigFactory}
import pureconfig.{ConfigReader, ConfigSource}
import pureconfig.generic.derivation.default.derived

case class ChannelConfig(
  logID: Option[DiscordID],
  spinlogID: Option[DiscordID],
) derives ConfigReader:
  def log: OptionT[IO, Channel] = getChannel("log", logID)
  def spinlog: OptionT[IO, Channel] = getChannel("spinlog", spinlogID)

object ChannelConfig:
  def fromConfig(config: Config = ConfigFactory.load()): ChannelConfig =
    ConfigSource.fromConfig(config).at("channels").loadOrThrow[ChannelConfig]