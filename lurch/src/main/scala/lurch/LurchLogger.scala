package lurch

import bot.{DiscordLogger, ChannelConfiguration as CommonChannelConfiguration}
import bot.model.{Channel, Discord, Message}
import cats.effect.{Deferred, IO, Ref}
import cats.data.OptionT
import org.typelevel.log4cats.Logger

class LurchLogger private(
  notificationsRef: Ref[IO, Set[String]],
  logChannelDeferred: Deferred[IO, Option[Channel]],
  spinlogChannelDeferred: Deferred[IO, Option[Channel]],
  val tortureChamberChannelDeferred: Deferred[IO, Option[Channel]],
) extends DiscordLogger(notificationsRef, logChannelDeferred, spinlogChannelDeferred):
  def logToTortureChamber(message: => String)(using Logger[IO]): OptionT[IO, Message] = log(tortureChamberChannelDeferred, message)

  def complete(
    discord: Discord,
    commonChannelConfiguration: CommonChannelConfiguration,
    channelConfiguration: ChannelConfiguration,
  )(using Logger[IO]): IO[Unit] =
    for
      logChannel <- DiscordLogger.getChannel(discord, "log", commonChannelConfiguration.logID)
      spinlogChannel <- DiscordLogger.getChannel(discord, "spinlog", commonChannelConfiguration.spinlogID)
      tortureChamberChannel <- DiscordLogger.getChannel(discord, "torture chamber", channelConfiguration.tortureChamberID)
      _ <- logChannelDeferred.complete(logChannel)
      _ <- spinlogChannelDeferred.complete(spinlogChannel)
      _ <- tortureChamberChannelDeferred.complete(tortureChamberChannel)
    yield ()


object LurchLogger:
  def create: IO[LurchLogger] =
    for
      notificationsRef <- Ref.of[IO, Set[String]](Set.empty)
      logChannelDeferred <- Deferred[IO, Option[Channel]]
      spinlogChannelDeferred <- Deferred[IO, Option[Channel]]
      tortureChamberChannelDeferred <- Deferred[IO, Option[Channel]]
    yield new LurchLogger(notificationsRef, logChannelDeferred, spinlogChannelDeferred, tortureChamberChannelDeferred)
