package bookofaifosi.model.event

import cats.effect.IO
import cats.syntax.traverse.*
import bookofaifosi.model.*
import cats.data.OptionT
import net.dv8tion.jda.api.JDA
import net.dv8tion.jda.api.entities.{MessageChannel, Guild as JDAGuild, Member as JDAMember, User as JDAUser}

import java.awt.image.BufferedImage
import java.io.{ByteArrayOutputStream, File}
import javax.imageio.ImageIO

abstract class Event(
  jdaChannel: MessageChannel,
  jdaAuthor: JDAUser,
  jdaMember: Option[JDAMember],
  jdaGuild: Option[JDAGuild],
):
  lazy val author: User = new User(jdaAuthor)
  lazy val authorMember: IO[Member] =
    OptionT.fromOption(jdaMember.map(new Member(_)))
      .orElseF(guild.traverse(author.member))
      .value
      .flatMap(IO.fromOption(_)(new Exception(s"Failed to get member of event")))
  lazy val channel: Channel = new Channel(jdaChannel)
  lazy val fromBot: Boolean = author.isBot
  lazy val guild: Option[Guild] = jdaGuild.map(new Guild(_))
  lazy val discord: Discord = new Discord(jdaAuthor.getJDA)

  def reply(string: String): IO[Unit] = channel.sendMessage(string).void

  def sendFile(file: File): IO[Message] = channel.sendFile(file)
