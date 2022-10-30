package lurch

import bot.*
import bot.model.{Channel, DiscordID}
import cats.data.OptionT
import cats.effect.IO
import cats.syntax.applicative.*
import cats.syntax.option.*
import cats.syntax.traverse.*
import com.typesafe.config.{Config, ConfigFactory}
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger
import pureconfig.generic.derivation.default.derived
import pureconfig.{ConfigReader, ConfigSource}

import java.time.Instant
import scala.concurrent.duration.FiniteDuration

given ConfigReader[DiscordID] = ConfigReader[Long].map(DiscordID.apply)

case class PilloryBitches(
  hours: Int,
  minutes: Int,
) derives ConfigReader

case class Roles(
  visitor: DiscordID,
  guest: DiscordID,
  keyholder: DiscordID,
  locked: DiscordID,
  switch: DiscordID,
  private val lastLockedThreshold: FiniteDuration,
  private val lastKeyheldThreshold: FiniteDuration,
) derives ConfigReader:
  def lastLockedCutoff: Instant = Instant.now.minusSeconds(lastLockedThreshold.toSeconds)
  def lastKeyheldCutoff: Instant = Instant.now.minusSeconds(lastKeyheldThreshold.toSeconds)

case class Channels(
  logID: Option[DiscordID],
  tortureChamberID: Option[DiscordID],
  spinlogID: Option[DiscordID],
) derives ConfigReader:
  def getChannel(chanelName: String, id: Option[DiscordID]): OptionT[IO, Channel] =
    OptionT {
      id.flatTraverse { id =>
        for
          given Logger[IO] <- Slf4jLogger.create[IO]
          discord <- Bot.discord.get
          channelEither <- discord.channelByID(id).attempt
          channel <- channelEither.fold(
            _ => Logger[IO].debug(s"$chanelName channel not configured.").as(None),
            _.some.pure[IO]
          )
        yield channel
      }
    }

  def log: OptionT[IO, Channel] = getChannel("log", logID)
  def tortureChamber: OptionT[IO, Channel] = getChannel("Torture Chamber", tortureChamberID)
  def spinlog: OptionT[IO, Channel] = getChannel("spinlog", spinlogID)

case class Configuration(
  checkFrequency: FiniteDuration,
  pilloryBitches: PilloryBitches,
  channels: Channels,
  roles: Roles,
) derives ConfigReader

object Configuration:
  def fromConfig(config: Config = ConfigFactory.load()): Configuration =
    ConfigSource.fromConfig(config).at("lurch").loadOrThrow[Configuration]
