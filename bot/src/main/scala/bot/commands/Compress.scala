package bot.commands

import bot.model.event.SlashCommandEvent
import bot.wheel.AddSegments.*

import cats.effect.IO
import org.typelevel.log4cats.Logger

object Compress extends SlashCommand with Options:
  /** If set to false only admins can see it by default.
    */
  override val isUserCommand: Boolean = true
  override val fullCommand: String    = "segment compress"

  override val options: List[PatternOption]                                                          = List(
    _.addOption[String]("segments", "Segments seperated by commas."),
  )
  override def apply(pattern: SlashPattern, event: SlashCommandEvent)(using Logger[IO]): IO[Boolean] =
    val segments = event.getOption[String]("segments")
    event
      .replyEphemeral(
        s"[$segments]".deflate.toOption.fold("Failed to compress segments")(deflated => s"AddSegments: $deflated"),
      )
      .as(true)

  override val description: String = "Compresses a list of segments so they can used for the AddSegments wheel command"
