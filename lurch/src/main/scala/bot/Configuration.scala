package bot

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
import pureconfig.module.catseffect.syntax.*

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

case class Configuration(
  checkFrequency: FiniteDuration,
  pilloryBitches: PilloryBitches,
  roles: Roles,
) derives ConfigReader

object Configuration:
  def fromConfig(config: Config = ConfigFactory.load()): IO[Configuration] =
    ConfigSource.fromConfig(config).at("lurch").loadF()
