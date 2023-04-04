package lurch

import bot.utils.getChannel
import bot.model.{Channel, DiscordID}
import cats.data.OptionT
import cats.effect.IO
import com.typesafe.config.{Config, ConfigFactory}
import pureconfig.{ConfigReader, ConfigSource}
import pureconfig.generic.derivation.default.derived

case class ChannelConfig(
  tortureChamberID: Option[DiscordID],
) derives ConfigReader:
  def tortureChamber: OptionT[IO, Channel] = getChannel("Torture Chamber", tortureChamberID)

object ChannelConfig:
  def fromConfig(config: Config = ConfigFactory.load()): ChannelConfig =
    ConfigSource.fromConfig(config).at("channels").loadOrThrow[ChannelConfig]
