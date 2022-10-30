package bot.model.event

import bot.commands.MacroHelper
import bot.model.Message
import cats.effect.IO
import cats.syntax.option.*
import bot.model.event.{Event, SlashCommandEvent}
import bot.syntax.action.*
import net.dv8tion.jda.api.entities.{MessageChannel, MessageEmbed, Guild as JDAGuild, Member as JDAMember, Message as JDAMessage, User as JDAUser}
import net.dv8tion.jda.api.events.interaction.command.SlashCommandInteractionEvent
import net.dv8tion.jda.api.interactions.InteractionHook as JDAInteractionHook
import net.dv8tion.jda.api.interactions.commands.OptionMapping

import java.awt.image.BufferedImage
import java.io.ByteArrayOutputStream
import javax.imageio.ImageIO
import scala.jdk.CollectionConverters.*

trait SlashAPI:
  def reply(string: String): IO[Message]
  def replyEphemeral(string: String): IO[Option[Message]]
  def replyImage(image: BufferedImage, title: String, ephemeral: Boolean = false): IO[Option[Message]]

class InteractionHook(
  underlying: JDAInteractionHook
) extends SlashAPI:
  override def reply(string: String): IO[Message] = underlying.sendMessage(string).toIO.map(Message(_))
  override def replyEphemeral(string: String): IO[Option[Message]] = underlying.sendMessage(string).toIO.map(Message(_).some)
  override def replyImage(image: BufferedImage, title: String, ephemeral: Boolean = false): IO[Option[Message]] =
    val outputStream = new ByteArrayOutputStream()
    ImageIO.write(image, "png", outputStream)
    underlying.sendFile(outputStream.toByteArray, title + ".png").toIO.map(Message(_).some)

object InteractionHook:
  given Conversion[JDAInteractionHook, InteractionHook] = hook => new InteractionHook(hook)

class SlashCommandEvent(
  jdaChannel: MessageChannel,
  jdaAuthor: JDAUser,
  jdaMember: Option[JDAMember],
  jdaGuild: Option[JDAGuild],
  underlying: SlashCommandInteractionEvent,
) extends GenericTextEvent(jdaChannel, jdaAuthor, jdaMember, jdaGuild) with SlashAPI:
  def deferReply(ephemeral: Boolean = false): IO[Unit] = underlying.deferReply(ephemeral).toIO.void
  override def reply(string: String): IO[Message] = underlying.reply(string).toIO.flatMap(_.retrieveOriginal.toIO).map(Message(_))
  override def replyEphemeral(string: String): IO[Option[Message]] = underlying.reply(string).setEphemeral(true).toIO.as(None)
  override def replyImage(image: BufferedImage, title: String, ephemeral: Boolean = false): IO[Option[Message]] =
    val outputStream = new ByteArrayOutputStream()
    ImageIO.write(image, "png", outputStream)
    underlying.reply(title).addFile(outputStream.toByteArray, title + ".png").setEphemeral(ephemeral).toIO.flatMap {
      case interactionHook if ephemeral => IO.pure(None)
      case interactionHook => interactionHook.retrieveOriginal.toIO.map(Message(_).some)
    }
  inline def getOption[T](option: String): T = MacroHelper.getOption[T](underlying, option)
  lazy val allOptions: List[OptionMapping] = underlying.getOptions.asScala.toList
  lazy val commandName: String = underlying.getName
  lazy val subCommandGroupName: Option[String] = Option(underlying.getSubcommandGroup)
  lazy val subCommandName: Option[String] = Option(underlying.getSubcommandName)
  lazy val fullCommand: String = List(Some(commandName), subCommandGroupName, subCommandName).flatten.mkString(" ")
  lazy val hook: InteractionHook = underlying.getHook

object SlashCommandEvent:
  given Conversion[SlashCommandInteractionEvent, SlashCommandEvent] = event =>
    new SlashCommandEvent(
      event.getChannel,
      event.getUser,
      Option(event.getMember),
      Option(event.getGuild),
      event,
    )
