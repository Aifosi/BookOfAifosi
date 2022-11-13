package bot.model

import java.io.File
import cats.effect.IO
import bot.syntax.action.*
import bot.syntax.io.*
import cats.data.OptionT
import net.dv8tion.jda.api.entities.{MessageChannel, MessageHistory}
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger

import scala.jdk.CollectionConverters.*

class Channel(channel: MessageChannel):
  lazy val discordID: DiscordID = channel.getIdLong
  lazy val name: String = channel.getName
  def sendMessage(string: String): IO[Message] = channel.sendMessage(string).toIO.map(new Message(_))
  def sendFile(file: File): IO[Message] = channel.sendFile(file).toIO.map(new Message(_))
  def findMessageByID(discordID: DiscordID): OptionT[IO, Message] =
    for
      given Logger[IO] <- OptionT.liftF(Slf4jLogger.create[IO])
      message <- OptionT(channel.retrieveMessageById(discordID.toLong).toIO.logErrorOption).map(new Message(_))
    yield message

  private val history: MessageHistory = channel.getHistory
  val lastHundred: IO[List[Message]] = history.retrievePast(100).toIO.map(_.asScala.toList.reverse.map(new Message(_)))
  val lastMessage: IO[Option[Message]] = history.retrievePast(1).toIO.map(_.asScala.headOption.map(new Message(_)))

  override def toString: String = channel.getAsMention
