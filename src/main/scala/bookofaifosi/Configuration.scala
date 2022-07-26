package bookofaifosi

import bookofaifosi.model.{Channel, DiscordID}
import com.typesafe.config.{Config, ConfigFactory}
import pureconfig.{ConfigReader, ConfigSource}
import pureconfig.generic.derivation.default.derived
import cats.syntax.traverse.*
import cats.effect.IO

import scala.concurrent.duration.FiniteDuration

given ConfigReader[DiscordID] = ConfigReader[Long].map(DiscordID.apply)

case class PilloryBitches(
  hours: Int,
  minutes: Int,
) derives ConfigReader

case class Roles(
  visitor: DiscordID,
  keyholder: DiscordID,
  locked: DiscordID,
) derives ConfigReader

case class Configuration(
  host: String,
  port: Int,
  publicHost: String,
  publicPort: Int,
  checkFrequency: FiniteDuration,
  pilloryBitches: PilloryBitches,
  logChannelId: Option[DiscordID],
  roles: Roles,
) derives ConfigReader:
  lazy val logChannel: IO[Option[Channel]] =
    logChannelId.flatTraverse { logChannelId =>
      for
        discord <- Bot.discord.get
        channelAttempt <- discord.channelByID(logChannelId).attempt
      yield channelAttempt.toOption
    }

object Configuration:
  def fromConfig(config: Config = ConfigFactory.load()): Configuration =
    ConfigSource.fromConfig(config).at("app").loadOrThrow[Configuration]
