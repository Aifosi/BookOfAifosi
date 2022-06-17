package bookofaifosi.commands

import cats.effect.IO
import bookofaifosi.commands.slash.SlashPattern
import bookofaifosi.wrappers.event.{Event, GenericTextEvent, MessageEvent, ReactionEvent, SlashCommandEvent}
import bookofaifosi.{Bot, Named}
import net.dv8tion.jda.api.interactions.commands.build.SlashCommandData
//import net.dv8tion.jda.api.interactions.commands.Command as JDACommand
import net.dv8tion.jda.api.interactions.commands.build.{CommandData, Commands}
import net.dv8tion.jda.api.utils.data.DataObject

import scala.util.matching.Regex

object Command:
  val all = ".+".r
  val userID = "(?:<@!)?(\\d+)(?:>)?".r
  val groupID = "(?:<@&)?(\\d+)(?:>)?".r

sealed abstract class Command[T, E <: Event] extends Named:
  def pattern: T

  protected def apply(pattern: T, event: E): IO[Boolean]

  val description: String

  def run(pattern: T, event: E, alwaysAllowed: Boolean): IO[Boolean] =
    val allowed = event.authorMember.fold(false) { member =>
      member.isGuildOwner ||
        member.roles.map(_.id).nonEmpty ||
        member.id == 211184778815340544L
    }
    if allowed || alwaysAllowed then apply(pattern, event) else IO.pure(false)

  override def toString: String = className

  def matches(event: E): Boolean

abstract class TextCommand extends Command[Regex, MessageEvent]:
  override def matches(event: MessageEvent): Boolean = pattern.matches(event.content)

abstract class SlashCommand extends Command[SlashPattern, SlashCommandEvent]:
  def command: String

  override lazy val pattern: SlashPattern = SlashPattern(command, description)

  override def matches(event: SlashCommandEvent): Boolean = command.equalsIgnoreCase(event.name)

trait Options:
  this: SlashCommand =>
  def options: List[SlashPattern => SlashPattern]

  override lazy val pattern: SlashPattern = options.foldLeft(SlashPattern(command, description))((command, option) => option(command))

abstract class ReactionCommand extends Command[String, ReactionEvent]:
  override def matches(event: ReactionEvent): Boolean = pattern == event.content
