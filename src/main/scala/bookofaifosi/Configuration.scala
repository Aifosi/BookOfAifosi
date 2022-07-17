package bookofaifosi

import bookofaifosi.model.{Channel, DiscordID}
import com.typesafe.config.{Config, ConfigFactory}
import pureconfig.{ConfigReader, ConfigSource}
import pureconfig.generic.derivation.default.derived
import cats.syntax.traverse.*
import cats.effect.IO

import scala.concurrent.duration.FiniteDuration

case class PilloryBitches(
    hours: Int,
    minutes: Int,
) derives ConfigReader

case class Configuration(
  host: String,
  port: Int,
  publicHost: String,
  publicPort: Int,
  checkFrequency: FiniteDuration,
  pilloryBitches: PilloryBitches,
  logChannelId: Option[DiscordID],
) derives ConfigReader:
  lazy val logChannel: IO[Option[Channel]] = Bot.config.logChannelId.traverse(logChannelId => Bot.discord.get.flatMap(_.channelByID(logChannelId)))

object Configuration:
  def fromConfig(config: Config = ConfigFactory.load()): Configuration =
    ConfigSource.fromConfig(config).at("app").loadOrThrow[Configuration]
