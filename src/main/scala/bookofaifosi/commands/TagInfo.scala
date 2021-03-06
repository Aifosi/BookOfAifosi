package bookofaifosi.commands

import bookofaifosi.Bot
import bookofaifosi.commands.Options.*
import bookofaifosi.db.TagRepository
import bookofaifosi.model.event.{AutoCompleteEvent, SlashCommandEvent}
import cats.effect.IO
import doobie.syntax.connectionio.*
import bookofaifosi.db.Filters.*
import cats.syntax.option.*
import org.typelevel.log4cats.Logger

object TagInfo extends SlashCommand with Options with AutoCompleteString:
  override val defaultEnabled: Boolean = true

  override val options: List[PatternOption] = List(
    Options.tagName
  )

  override val autoCompleteOptions: Map[String, AutoCompleteEvent => IO[List[String]]] = Map(
    "name" -> (_ => TagRepository.list().map(_.map(_.name)))
  )
  
  override val fullCommand: String = "tag info"

  override def apply(pattern: SlashPattern, event: SlashCommandEvent)(using Logger[IO]): IO[Boolean] =
    val tagName = event.getOption[String]("name")
    for
      tag <- TagRepository.find(tagName.some.equalName)
      _ <- event.reply(tag.fold(s"Could not find tag named \"$tagName\"") { tag =>
        s"name: ${tag.name}${tag.description.fold(" No Description")(description => s" description: $description")}"
      })
    yield true

  override val description: String = "Prints information of a tag"
