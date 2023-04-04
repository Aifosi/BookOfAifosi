package bot

import bot.model.{Channel, DiscordID}
import bot.syntax.io.*
import bot.syntax.stream.*
import cats.data.{EitherT, OptionT}
import cats.effect.{IO, Ref}
import cats.syntax.applicative.*
import cats.syntax.option.*
import cats.syntax.traverse.*
import fs2.Stream
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger
import scala.concurrent.duration.*

object utils:
  def getChannel(chanelName: String, id: Option[DiscordID]): OptionT[IO, Channel] =
    OptionT {
      id.flatTraverse { id =>
        for
          given Logger[IO] <- Slf4jLogger.create[IO]
          discord <- Bot.discord.get
          channelEither <- discord.channelByID(id).value
          channel <- channelEither.fold(
            _ => Logger[IO].debug(s"$chanelName channel not configured.").as(None),
            _.some.pure[IO]
          )
        yield channel
      }
    }
    
  type Maybe[T] = EitherT[IO, Exception, T]

  def log(message: => String)(using Logger[IO]): Stream[IO, Nothing] =
    Stream.eval(Logger[IO].info(message) *> Bot.channels.log.sendMessage(message).value).drain
  
  def logWithoutSpam(notificationsRef: Ref[IO, Set[String]])(message: => String)(using Logger[IO]): Stream[IO, 
    Nothing] =
    for
      notifications <- notificationsRef.get.streamed
      _ <- Stream.filter(!notifications.contains(message))
      _ <- notificationsRef.update(_ + message).streamed
      _ <- (IO.sleep(1.hour) *> notificationsRef.update(_ - message)).start.streamed
      n <- log(message)
    yield n