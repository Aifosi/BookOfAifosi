package bookofaifosi.commands

import bookofaifosi.Bot
import bookofaifosi.commands.PatternOption
import bookofaifosi.model.event.SlashCommandEvent
import bookofaifosi.db.TagRepository
import cats.effect.IO
import doobie.syntax.connectionio.*

object TagAdd extends SlashCommand with Options:
  override val defaultEnabled: Boolean = false

  override val fullCommand: String = "tag add"

  override val options: List[PatternOption] = List(
    _.addOption[String]("name", "Name of the tag you want to add."),
    _.addOption[Option[String]]("description", "Description of this tag."),
  )

  override def apply(pattern: SlashPattern, event: SlashCommandEvent): IO[Boolean] =
    val tag = event.getOption[String]("name")
    val description = event.getOption[Option[String]]("description")
    for
      _ <- TagRepository.add(tag, description)
      _ <- event.reply(s"Tag \"$tag\" added.")
    yield true

  override val description: String = "Adds a new tag."
