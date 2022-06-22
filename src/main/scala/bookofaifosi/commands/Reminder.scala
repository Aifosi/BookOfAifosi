package bookofaifosi.commands

import bookofaifosi.commands.Options.*
import bookofaifosi.model.event.{AutoCompleteEvent, SlashCommandEvent}
import cats.effect.IO
import cats.syntax.applicative.*

import java.util.concurrent.TimeUnit
import scala.concurrent.duration.*

object Reminder extends SlashCommand with Options with AutoCompleteString:
  override val defaultEnabled: Boolean = true
  override val fullCommand: String = "reminder create"
  override val options: List[PatternOption] = List(
    _.addOption[String]("name", "Name of the reminder."),
    Options.duration("duration"),
    Options.timeUnit,
    _.addOption[String]("message", "Message of the reminder."),
  )

  override val autoCompleteOptions: Map[String, AutoCompleteEvent => IO[List[String]]] = Map(
    AutoComplete.timeUnit,
  )

  override def apply(pattern: SlashPattern, event: SlashCommandEvent): IO[Boolean] =
    val name = event.getOption[String]("name")
    val duration = event.getOption[Long]("duration")
    val unit = event.getOption[String]("unit")
    val message = event.getOption[String]("message")
    AutoComplete.timeUnits.get(unit).fold(event.replyEphemeral(s"Invalid unit \"$unit\"").as(true)) { unit =>
      for
        _ <- (IO.sleep(FiniteDuration(duration, unit)) *> event.author.sendMessage(message)).start
        _ <- event.replyEphemeral("Reminder created")
      yield true
    }

  override val description: String = "Creates a reminder that will be sent when the given time has passed."
