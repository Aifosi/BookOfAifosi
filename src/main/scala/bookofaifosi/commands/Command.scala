package bookofaifosi.commands

import cats.effect.{IO, Ref}
import bookofaifosi.model.event.{AutoCompleteEvent, Event, GenericTextEvent, MessageEvent, ReactionEvent, SlashAPI, SlashCommandEvent}
import bookofaifosi.{Bot, Named}
import bookofaifosi.commands.SlashPattern.*
import net.dv8tion.jda.api.interactions.commands.build.SlashCommandData
import net.dv8tion.jda.api.interactions.commands.build.{CommandData, Commands}
import net.dv8tion.jda.api.utils.data.DataObject
import bookofaifosi.commands.Options.PatternOptions
import fs2.Stream
import scala.concurrent.duration.*
import scala.util.matching.Regex

object Command:
  val all = ".+".r
  val userID = "(?:<@!)?(\\d+)(?:>)?".r
  val groupID = "(?:<@&)?(\\d+)(?:>)?".r

type AnyCommand = Command[?, ? <: Event]

sealed abstract class Command[T, E <: Event] extends Named:
  def pattern: T

  def apply(pattern: T, event: E): IO[Boolean]

  val description: String

  override def toString: String = className

  def matches(event: E): Boolean

abstract class TextCommand extends Command[Regex, MessageEvent]:
  override def matches(event: MessageEvent): Boolean = pattern.matches(event.content)

abstract class ReactionCommand extends Command[String, ReactionEvent]:
  override def matches(event: ReactionEvent): Boolean = pattern == event.content

abstract class SlashCommand extends Command[SlashPattern, SlashCommandEvent]:
  val defaultEnabled: Boolean

  val fullCommand: String

  final lazy val (command: String, subCommandGroup: Option[String], subCommand: Option[String]) = fullCommand.split(" ").toList match {
    case List(command, subCommandGroup, subCommand) => (command, Some(subCommandGroup), Some(subCommand))
    case List(command, subCommand) => (command, None, Some(subCommand))
    case List(command)             => (command, None, None)
    case _ => throw new Exception(s"Invalid command $fullCommand")
  }

  final protected lazy val slashPattern: SlashPattern =
    SlashPattern(command, subCommandGroup, subCommand, description, defaultEnabled)

  override lazy val pattern: SlashPattern = slashPattern

  override def matches(event: SlashCommandEvent): Boolean =
    List(Some(command), subCommandGroup, subCommand).flatten.mkString(" ").equalsIgnoreCase(fullCommand)

trait Streams:
  this: AnyCommand =>
  def stream: Stream[IO, Unit]


trait Options:
  this: SlashCommand =>
  val options: List[PatternOptions]

  override lazy val pattern: SlashPattern = slashPattern.applyOptions(options)

object Options:
  type PatternOptions = SlashPattern => SlashPattern

type AutoCompletable = AutoComplete[?]

sealed trait AutoComplete[T]:
  this: SlashCommand =>
  val autoCompleteOptions: Map[String, AutoCompleteEvent => IO[List[T]]]

  def matchesAutoComplete(event: AutoCompleteEvent): Boolean = fullCommand.equalsIgnoreCase((event.commandName +: event.subCommandName.toList).mkString(" "))

  protected def focusedOptions(event: AutoCompleteEvent): IO[List[T]] = autoCompleteOptions.get(event.focusedOption).fold(IO.pure(List.empty))(_(event))

  inline protected def reply(event: AutoCompleteEvent): IO[Boolean] = focusedOptions(event).flatMap(event.replyChoices[T](_)).as(true)

  def apply(event: AutoCompleteEvent): IO[Boolean]

trait AutoCompleteString extends AutoComplete[String]:
  this: SlashCommand =>
  override def apply(event: AutoCompleteEvent): IO[Boolean] =
    focusedOptions(event).flatMap { options =>
      event.replyChoices[String](options.filter(_.toLowerCase.startsWith(event.focusedValue.toLowerCase)))
    }.as(true)


trait AutoCompleteInt extends AutoComplete[Int]:
  this: SlashCommand =>
  override def apply(event: AutoCompleteEvent): IO[Boolean] = reply(event)

trait AutoCompleteLong extends AutoComplete[Long]:
  this: SlashCommand =>
  override def apply(event: AutoCompleteEvent): IO[Boolean] = reply(event)

trait AutoCompleteDouble extends AutoComplete[Double]:
  this: SlashCommand =>
  override def apply(event: AutoCompleteEvent): IO[Boolean] = reply(event)

trait SlowResponse:
  this: SlashCommand =>
  val ephemeralResponses: Boolean
  def slowResponse(pattern: SlashPattern, event: SlashCommandEvent, slashAPI: Ref[IO, SlashAPI]): IO[Unit]

  override final def apply(pattern: SlashPattern, event: SlashCommandEvent): IO[Boolean] =
    def switchToHook(slashAPI: Ref[IO, SlashAPI], repliedRef: Ref[IO, Boolean]) =
      for
        _ <- IO.sleep(3.seconds)
        replied <- repliedRef.get
        _ <- if !replied then
          slashAPI.set(event.hook) *> event.deferReply(ephemeralResponses) *> repliedRef.set(true)
        else
          IO.unit
      yield ()

    for
      slashAPI <- Ref.of[IO, SlashAPI](event)
      repliedRef <- Ref.of[IO, Boolean](false)
      _ <- switchToHook(slashAPI, repliedRef).start
      _ <- slowResponse(pattern, event, slashAPI)
      _ <- repliedRef.set(true)
    yield true
