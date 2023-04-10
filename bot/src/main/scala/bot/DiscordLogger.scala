package bot

import bot.model.{Channel, DiscordID, Discord, Message}
import bot.syntax.io.*
import bot.syntax.stream.*
import cats.effect.{IO, Ref, Deferred}
import cats.data.OptionT
import cats.syntax.option.*
import cats.syntax.applicative.*
import cats.syntax.traverse.*
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger
import fs2.Stream
import scala.concurrent.duration.*

open class DiscordLogger protected(
  notificationsRef: Ref[IO, Set[String]],
  logChannelDeferred: Deferred[IO, Option[Channel]],
  spinlogChannelDeferred: Deferred[IO, Option[Channel]],
):
  protected def log(channel: Deferred[IO, Option[Channel]], message: => String)(using Logger[IO]): OptionT[IO, Message] =
    OptionT(channel.get).semiflatMap(_.sendMessage(message))

  protected def logStream(channel: Deferred[IO, Option[Channel]], message: => String)(using Logger[IO]): Stream[IO, Nothing] =
    Stream.eval(log(channel, message).value).drain

  def logToChannel(message: => String)(using Logger[IO]): IO[Unit] = log(logChannelDeferred, message).value.void
  def logToSpinlog(message: => String)(using Logger[IO]): IO[Unit] = log(spinlogChannelDeferred, message).value.void

  def logToChannelStream(message: => String)(using Logger[IO]): Stream[IO, Nothing] = logStream(logChannelDeferred, message)
  def logToSpinlogStream(message: => String)(using Logger[IO]): Stream[IO, Nothing] = logStream(spinlogChannelDeferred, message)

  def logWithoutSpam(message: => String)(using Logger[IO]): Stream[IO, Nothing] =
    for
      notifications <- notificationsRef.get.streamed
      _ <- Stream.filter(!notifications.contains(message))
      _ <- notificationsRef.update(_ + message).streamed
      _ <- (IO.sleep(1.hour) *> notificationsRef.update(_ - message)).start.streamed
      n <- logToChannelStream(message)
    yield n
    
  def complete(discord: Discord, channelConfig: ChannelConfiguration)(using Logger[IO]): IO[Unit] =
    for
      logChannel <- DiscordLogger.getChannel(discord, "log", channelConfig.logID)
      spinlogChannel <- DiscordLogger.getChannel(discord, "spinlog", channelConfig.spinlogID)
      _ <- logChannelDeferred.complete(logChannel)
      _ <- spinlogChannelDeferred.complete(spinlogChannel)
    yield ()

object DiscordLogger:
  def getChannel(discord: Discord, chanelName: String, id: Option[DiscordID])(using Logger[IO]): IO[Option[Channel]] =
    id.flatTraverse { id =>
      discord.channelByID(id).toOption.flatTapNone(Logger[IO].debug(s"$chanelName channel not configured.")).value
    }

  def create: IO[DiscordLogger] =
    for
      notificationsRef <- Ref.of[IO, Set[String]](Set.empty)
      logChannelDeferred <- Deferred[IO, Option[Channel]]
      spinlogChannelDeferred <- Deferred[IO, Option[Channel]]
    yield new DiscordLogger(notificationsRef, logChannelDeferred, spinlogChannelDeferred)
