package bookofaifosi.commands

import bookofaifosi.model.event.AutoCompleteEvent
import cats.effect.IO
import cats.syntax.applicative.*

import java.util.concurrent.TimeUnit

object AutoComplete:
  type AutoCompleteOption = (String, AutoCompleteEvent => IO[List[String]])
  private val timeUnits: Map[String, TimeUnit] = TimeUnit.values.toList.map { unit =>
    unit.toString.toLowerCase -> unit
  }.toMap

  lazy val timeUnit: AutoCompleteOption = "unit" -> (_ => timeUnits.keys.toList.pure)

sealed trait AutoComplete[T]:
  this: SlashCommand =>
  val autoCompleteOptions: Map[String, AutoCompleteEvent => IO[List[T]]]

  def matchesAutoComplete(event: AutoCompleteEvent): Boolean = fullCommand.equalsIgnoreCase((event.commandName 
    +: event.subCommandName.toList).mkString(" "))

  protected def focusedOptions(event: AutoCompleteEvent): IO[List[T]] =
    autoCompleteOptions.get(event.focusedOption).fold(IO.pure(List.empty))(_(event))

  inline protected def reply(event: AutoCompleteEvent): IO[Boolean] = focusedOptions(event).flatMap(event
    .replyChoices[T](_)).as(true)

  def apply(event: AutoCompleteEvent): IO[Boolean]

type AutoCompletable = AutoComplete[?]

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