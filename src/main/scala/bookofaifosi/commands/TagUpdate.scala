package bookofaifosi.commands

import bookofaifosi.Bot
import bookofaifosi.commands.Options.PatternOptions
import bookofaifosi.wrappers.event.SlashCommandEvent
import bookofaifosi.db.Tag
import cats.effect.IO
import doobie.syntax.connectionio.*

object TagUpdate extends SlashCommand with Options with AutoCompleteString:
  override val defaultEnabled: Boolean = false

  override val fullCommand: String = "tag update"

  override val options: List[PatternOptions] = List(
    _.addOption[String]("name", "The name of the tag you want to update.", autoComplete = true),
    _.addOption[Option[String]]("new_name", "The new name for the tag."),
    _.addOption[Option[String]]("new_description", "The description for the tag."),
    _.addOption[Option[Boolean]]("remove_description", "Should the description be updated?"),
  )

  override val autoCompleteOptions: Map[String, IO[List[String]]] = Map(
    "name" -> Tag.list().transact(Bot.xa).map(_.map(_.name))
  )

  override def apply(pattern: SlashPattern, event: SlashCommandEvent): IO[Boolean] =
    val name = event.getOption[String]("name")
    val newName = event.getOption[Option[String]]("new_name")
    val newDescription = event.getOption[Option[String]]("new_description")
    val removeDescription = event.getOption[Option[Boolean]]("remove_description").getOrElse(false)
    val descriptionUpdate = if removeDescription then Some(None) else newDescription.map(Some(_))
    for
      updated <- Tag.update(name, newName, descriptionUpdate).transact(Bot.xa)
      message = updated match {
        case 0 => s"No tags named \n$name\n found!"
        case 1 => s"$name updated."
        case x => s"Updated $x tags."
      }
      _ <- event.reply(message)
    yield true

  override val description: String = "Updates a tag's name and/or description."