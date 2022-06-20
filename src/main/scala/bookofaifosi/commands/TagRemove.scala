package bookofaifosi.commands

import bookofaifosi.Bot
import bookofaifosi.commands.Options.PatternOptions
import bookofaifosi.wrappers.event.SlashCommandEvent
import bookofaifosi.db.Tag
import cats.effect.IO
import doobie.syntax.connectionio.*

object TagRemove extends SlashCommand with Options with AutoCompleteString:
  override val defaultEnabled: Boolean = false

  override val fullCommand: String = "tag remove"

  override val options: List[PatternOptions] = List(
    _.addOption[String]("name", "The name of the tags you want to remove.", autoComplete = true),
  )

  override val autoCompleteOptions: Map[String, IO[List[String]]] = Map(
    "name" -> Tag.list().transact(Bot.xa).map(_.map(_.name))
  )

  override def apply(pattern: SlashPattern, event: SlashCommandEvent): IO[Boolean] =
    val tag = event.getOption[String]("name")
    for
      removed <- Tag.remove(tag).transact(Bot.xa)
      message = removed match {
        case 0 => s"No tags named \n$tag\n found!"
        case 1 => s"$tag removed."
        case x => s"Removed $x tags."
      }
      _ <- event.reply(message)
    yield true

  override val description: String = "Deletes all tags with the given name."
