package bookofaifosi.commands

import bookofaifosi.Bot
import bookofaifosi.commands.Options.PatternOptions
import bookofaifosi.db.Tag
import bookofaifosi.wrappers.event.SlashCommandEvent
import cats.effect.IO
import doobie.syntax.connectionio.*

object TagInfo extends SlashCommand with Options with AutoCompleteString:
  override val defaultEnabled: Boolean = true

  override val options: List[PatternOptions] = List(
    _.addOption[String]("name", "Name of the tag you want to search.", autoComplete = true)
  )

  override val autoCompleteOptions: Map[String, IO[List[String]]] = Map(
    "name" -> Tag.list().transact(Bot.xa).map(_.map(_.name))
  )
  
  override val fullCommand: String = "tag info"

  override def apply(pattern: SlashPattern, event: SlashCommandEvent): IO[Boolean] =
    val tagName = event.getOption[String]("name")
    for
      tag <- Tag.find(tagName).transact(Bot.xa)
      _ <- event.reply(tag.fold(s"Could not find tag named \"$tagName\"") { tag =>
        s"name: ${tag.name}${tag.description.fold(" No Description")(description => s" description: $description")}"
      })
    yield true

  override val description: String = "Prints information of a tag"
