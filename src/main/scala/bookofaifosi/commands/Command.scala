package bookofaifosi.commands

import cats.effect.IO
import bookofaifosi.wrappers.event.{AutoCompleteEvent, Event, GenericTextEvent, MessageEvent, ReactionEvent, SlashCommandEvent}
import bookofaifosi.{Bot, Named}
import net.dv8tion.jda.api.interactions.commands.build.SlashCommandData
import net.dv8tion.jda.api.interactions.commands.build.{CommandData, Commands}
import net.dv8tion.jda.api.utils.data.DataObject
import bookofaifosi.commands.Options.PatternOptions

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

  final lazy val (command: String, subCommand: Option[String]) = fullCommand.split(" ").toList match {
    case List(command, subCommand) => (command, Some(subCommand))
    case List(command)             => (command, None)
    case _ => throw new Exception(s"Invalid command $fullCommand")
  }

  final protected lazy val slashPattern: SlashPattern = SlashPattern(command, description, subCommand.map(_ -> description).toSet)

  override lazy val pattern: SlashPattern = slashPattern

  override def matches(event: SlashCommandEvent): Boolean = fullCommand.equalsIgnoreCase((event.commandName +: event.subCommandName.toList).mkString(" "))


trait Options:
  this: SlashCommand =>
  val options: List[PatternOptions]

  override lazy val pattern: SlashPattern = options.foldLeft(slashPattern)((command, option) => option(command)(subCommand))

object Options:
  type PatternOptions = SlashPattern => Option[String] => SlashPattern

type AutoCompletable = AutoComplete[?]

sealed trait AutoComplete[T]:
  this: SlashCommand =>
  val autoCompleteOptions: Map[String, IO[List[T]]]

  def matchesAutoComplete(event: AutoCompleteEvent): Boolean = fullCommand.equalsIgnoreCase((event.commandName +: event.subCommandName.toList).mkString(" "))

  protected def focusedOptions(event: AutoCompleteEvent): IO[List[T]] = autoCompleteOptions.getOrElse(event.focusedOption, IO.pure(List.empty))

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

