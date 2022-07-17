package bookofaifosi.commands

import bookofaifosi.Bot
import bookofaifosi.commands.Options.*
import bookofaifosi.model.event.{AutoCompleteEvent, SlashCommandEvent}
import bookofaifosi.db.TagRepository
import bookofaifosi.db.Filters.*
import cats.effect.IO
import doobie.syntax.connectionio.*
import org.typelevel.log4cats.Logger

object TagRemove extends SlashCommand with Options with AutoCompleteString:
  override val defaultEnabled: Boolean = false

  override val fullCommand: String = "tag remove"

  override val options: List[PatternOption] = List(
    Options.tagName
  )

  override val autoCompleteOptions: Map[String, AutoCompleteEvent => IO[List[String]]] = Map(
    "name" -> (_ => TagRepository.list().map(_.map(_.name)))
  )

  override def apply(pattern: SlashPattern, event: SlashCommandEvent)(using Logger[IO]): IO[Boolean] =
    val tag = event.getOption[String]("name")
    for
      removed <- TagRepository.remove(tag.equalName)
      message = removed match {
        case 0 => s"No tags named \n$tag\n found!"
        case 1 => s"$tag removed."
        case x => s"Removed $x tags."
      }
      _ <- event.reply(message)
    yield true

  override val description: String = "Deletes all tags with the given name."
