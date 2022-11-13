package bot

import bot.model.{Channel, DiscordID}
import cats.data.OptionT
import cats.effect.IO
import cats.syntax.applicative.*
import cats.syntax.option.*
import cats.syntax.traverse.*
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger

object Utils:
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
