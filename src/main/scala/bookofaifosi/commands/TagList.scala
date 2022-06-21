package bookofaifosi.commands

import bookofaifosi.Bot
import bookofaifosi.commands.PatternOption
import bookofaifosi.db.TagRepository
import bookofaifosi.model.event.SlashCommandEvent
import cats.effect.IO
import doobie.syntax.connectionio.*

object TagList extends SlashCommand:
  override val defaultEnabled: Boolean = true

  override val fullCommand: String = "tag list"

  override def apply(pattern: SlashPattern, event: SlashCommandEvent): IO[Boolean] =
    for
      tags <- TagRepository.list()
      _ <- event.reply(s"Current tags: ${tags.map(_.name).mkString(", ")}")
    yield true

  override val description: String = "Prints the list of all existing tag"
