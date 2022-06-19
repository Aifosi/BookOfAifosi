package bookofaifosi.commands

import cats.effect.IO
import bookofaifosi.wrappers.event.{Event, GenericTextEvent, MessageEvent, ReactionEvent, SlashCommandEvent}
import bookofaifosi.{Bot, Named}
import net.dv8tion.jda.api.interactions.commands.build.SlashCommandData
import net.dv8tion.jda.api.interactions.commands.build.{CommandData, Commands}
import net.dv8tion.jda.api.utils.data.DataObject

import scala.util.matching.Regex

object Command:
  val all = ".+".r
  val userID = "(?:<@!)?(\\d+)(?:>)?".r
  val groupID = "(?:<@&)?(\\d+)(?:>)?".r

sealed abstract class Command[T, E <: Event] extends Named:
  def pattern: T

  def apply(pattern: T, event: E): IO[Boolean]

  val description: String

  override def toString: String = className

  def matches(event: E): Boolean

abstract class TextCommand extends Command[Regex, MessageEvent]:
  override def matches(event: MessageEvent): Boolean = pattern.matches(event.content)

abstract class SlashCommand extends Command[SlashPattern, SlashCommandEvent]:
  val defaultEnabled: Boolean

  val fullCommand: String

  final protected lazy val (command: String, subCommand: Option[String]) = fullCommand.split(" ").toList match {
    case List(command, subCommand) => (command, Some(subCommand))
    case List(command)             => (command, None)
    case _ => throw new Exception(s"Invalid command $fullCommand")
  }

  final protected lazy val slashPattern: SlashPattern = SlashPattern(command, description, subCommand.toSet)

  override lazy val pattern: SlashPattern = slashPattern

  override def matches(event: SlashCommandEvent): Boolean = fullCommand.equalsIgnoreCase((event.commandName +: event.subCommandName.toList).mkString(" "))

trait Options:
  this: SlashCommand =>
  type PatternOptions = SlashPattern => Option[String] => SlashPattern
  val options: List[PatternOptions]

  override lazy val pattern: SlashPattern = options.foldLeft(slashPattern)((command, option) => option(command)(subCommand))

abstract class ReactionCommand extends Command[String, ReactionEvent]:
  override def matches(event: ReactionEvent): Boolean = pattern == event.content
