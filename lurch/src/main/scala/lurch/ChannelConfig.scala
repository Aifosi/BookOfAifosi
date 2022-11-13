package lurch

import bot.Utils.getChannel
import bot.model.{Channel, DiscordID}
import cats.data.OptionT
import cats.effect.IO
import com.typesafe.config.{Config, ConfigFactory}
import pureconfig.{ConfigReader, ConfigSource}
import pureconfig.generic.derivation.default.derived

case class ChannelConfig(
  tortureChamberID: Option[DiscordID],
  spinlogID: Option[DiscordID],
) derives ConfigReader:
  def tortureChamber: OptionT[IO, Channel] = getChannel("Torture Chamber", tortureChamberID)
  def spinlog: OptionT[IO, Channel] = getChannel("spinlog", spinlogID)

object ChannelConfig:
  def fromConfig(config: Config = ConfigFactory.load()): ChannelConfig =
    ConfigSource.fromConfig(config).at("channels").loadOrThrow[ChannelConfig]