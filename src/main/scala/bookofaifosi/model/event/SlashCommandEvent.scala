package bookofaifosi.model.event

import bookofaifosi.commands.MacroHelper
import cats.effect.IO
import bookofaifosi.model.event.{Event, SlashCommandEvent}
import bookofaifosi.syntax.action.*
import net.dv8tion.jda.api.entities.{MessageChannel, MessageEmbed, Guild as JDAGuild, Member as JDAMember, Message as JDAMessage, User as JDAUser}
import net.dv8tion.jda.api.events.interaction.command.SlashCommandInteractionEvent
import net.dv8tion.jda.api.interactions.InteractionHook as JDAInteractionHook

import java.awt.image.BufferedImage
import java.io.ByteArrayOutputStream
import javax.imageio.ImageIO

trait SlashAPI:
  def reply(string: String): IO[Unit]
  def replyEphemeral(string: String): IO[Unit]
  def replyImage(image: BufferedImage, title: String, ephemeral: Boolean = false): IO[Unit]

class InteractionHook(
  underlying: JDAInteractionHook
) extends SlashAPI:
  def reply(string: String): IO[Unit] = underlying.sendMessage(string).toIO.void
  def replyEphemeral(string: String): IO[Unit] = underlying.sendMessage(string).toIO.void
  def replyImage(image: BufferedImage, title: String, ephemeral: Boolean = false): IO[Unit] =
    val outputStream = new ByteArrayOutputStream()
    ImageIO.write(image, "png", outputStream)
    underlying.sendFile(outputStream.toByteArray, title + ".png").toIO.void

object InteractionHook:
  given Conversion[JDAInteractionHook, InteractionHook] = hook => new InteractionHook(hook)

class SlashCommandEvent(
  jdaChannel: MessageChannel,
  jdaAuthor: JDAUser,
  jdaMember: Option[JDAMember],
  jdaGuild: Option[JDAGuild],
  underlying: SlashCommandInteractionEvent,
) extends GenericTextEvent(jdaChannel, jdaAuthor, jdaMember, jdaGuild) with SlashAPI:
  override def reply(string: String): IO[Unit] = underlying.reply(string).toIO.void
  def deferReply(ephemeral: Boolean = false): IO[Unit] = underlying.deferReply(ephemeral).toIO.void
  def replyEphemeral(string: String): IO[Unit] = underlying.reply(string).setEphemeral(true).toIO.void
  def replyImage(image: BufferedImage, title: String, ephemeral: Boolean = false): IO[Unit] =
    val outputStream = new ByteArrayOutputStream()
    ImageIO.write(image, "png", outputStream)
    underlying.reply(title).addFile(outputStream.toByteArray, title + ".png").setEphemeral(ephemeral).toIO.void
  inline def getOption[T](option: String): T = MacroHelper.getOption[T](underlying, option)
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
