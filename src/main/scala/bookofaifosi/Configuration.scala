package bookofaifosi

import bookofaifosi.model.{Channel, DiscordID}
import com.typesafe.config.{Config, ConfigFactory}
import pureconfig.{ConfigReader, ConfigSource}
import pureconfig.generic.derivation.default.derived
import cats.syntax.traverse.*
import cats.effect.IO

import java.time.Instant
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
  switch: DiscordID,
  private val lastLockedThreshold: FiniteDuration,
  private val lastKeyheldThreshold: FiniteDuration,
) derives ConfigReader:
  def lastLockedCutoff: Instant = Instant.now.minusSeconds(lastLockedThreshold.toSeconds)
  def lastKeyheldCutoff: Instant = Instant.now.minusSeconds(lastKeyheldThreshold.toSeconds)

case class Configuration(
  host: String,
  port: Int,
  publicHost: String,
  publicPort: Int,
  checkFrequency: FiniteDuration,
  pilloryBitches: PilloryBitches,
  logChannelId: Option[DiscordID],
  tortureChamberChannelId: Option[DiscordID],
  roles: Roles,
) derives ConfigReader:
  def getChannel(id: Option[DiscordID]): IO[Option[Channel]] =
    id.flatTraverse { id =>
      for
        discord <- Bot.discord.get
        channelAttempt <- discord.channelByID(id).attempt
      yield channelAttempt.toOption
    }
  
  lazy val logChannel: IO[Option[Channel]] = getChannel(logChannelId)
  
  lazy val tortureChamberChannel: IO[Option[Channel]] = getChannel(tortureChamberChannelId)

object Configuration:
  def fromConfig(config: Config = ConfigFactory.load()): Configuration =
    ConfigSource.fromConfig(config).at("app").loadOrThrow[Configuration]
