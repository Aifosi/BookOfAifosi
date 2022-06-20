package bookofaifosi.commands

import bookofaifosi.Bot
import bookofaifosi.commands.Options.PatternOptions
import bookofaifosi.db.TagRepository
import bookofaifosi.model.event.{AutoCompleteEvent, SlashCommandEvent}
import cats.effect.IO
import doobie.syntax.connectionio.*
import bookofaifosi.db.Filters.*
import cats.syntax.option.*

object TagInfo extends SlashCommand with Options with AutoCompleteString:
  override val defaultEnabled: Boolean = true

  override val options: List[PatternOptions] = List(
    _.addOption[String]("name", "Name of the tag you want to search.", autoComplete = true)
  )

  override val autoCompleteOptions: Map[String, AutoCompleteEvent => IO[List[String]]] = Map(
    "name" -> (_ => TagRepository.list().map(_.map(_.name)))
  )
  
  override val fullCommand: String = "tag info"

  override def apply(pattern: SlashPattern, event: SlashCommandEvent): IO[Boolean] =
    val tagName = event.getOption[String]("name")
    for
      tag <- TagRepository.find(tagName.some.equalName)
      _ <- event.reply(tag.fold(s"Could not find tag named \"$tagName\"") { tag =>
        s"name: ${tag.name}${tag.description.fold(" No Description")(description => s" description: $description")}"
      })
    yield true

  override val description: String = "Prints information of a tag"
