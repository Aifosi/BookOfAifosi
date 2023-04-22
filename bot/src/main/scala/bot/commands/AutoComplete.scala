package bot.commands

import bot.model.event.AutoCompleteEvent

import cats.effect.IO
import cats.syntax.applicative.*
import java.util.concurrent.TimeUnit

object AutoComplete:
  type AutoCompleteOption = (String, AutoCompleteEvent => IO[List[String]])
  val timeUnits: Map[String, TimeUnit] = Map(
    TimeUnit.SECONDS.toString.toLowerCase -> TimeUnit.SECONDS,
    TimeUnit.MINUTES.toString.toLowerCase -> TimeUnit.MINUTES,
    TimeUnit.HOURS.toString.toLowerCase   -> TimeUnit.HOURS,
    TimeUnit.DAYS.toString.toLowerCase    -> TimeUnit.DAYS,
  )

  lazy val timeUnit: AutoCompleteOption = "unit" -> (_ => timeUnits.keys.toList.pure)

sealed trait AutoComplete[T]:
  this: SlashCommand =>
  val autoCompleteOptions: Map[String, AutoCompleteEvent => IO[List[T]]]

  def matchesAutoComplete(event: AutoCompleteEvent): Boolean =
    event.fullCommand.equalsIgnoreCase(fullCommand)

  protected def focusedOptions(event: AutoCompleteEvent): IO[List[T]] =
    autoCompleteOptions.get(event.focusedOption).fold(IO.pure(List.empty))(_(event))

  protected inline def reply(event: AutoCompleteEvent): IO[Boolean] = focusedOptions(event)
    .flatMap(
      event
        .replyChoices[T](_),
    )
    .as(true)

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
