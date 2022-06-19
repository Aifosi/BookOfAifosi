package bookofaifosi.commands

import bookofaifosi.wrappers.event.SlashCommandEvent
import bookofaifosi.db.Tag
import cats.effect.IO

object AddTag extends SlashCommand with Options:
  override val defaultEnabled: Boolean = false

  override val fullCommand: String = "tag add"

  override val options: List[PatternOptions] = List(
    _.addOption[String]("tag", "The tag you want to add"),
    _.addOption[Option[String]]("description", "Description of this tag"),
  )

  override def apply(pattern: SlashPattern, event: SlashCommandEvent): IO[Boolean] =
    val tag = event.getOption[String]("tag")
    val description = event.getOption[Option[String]]("description")
    for
      _ <- Tag.add(tag, description)
      _ <- event.reply("Tag added.")
    yield true

  override val description: String = "Adds a new tag"
