package bookofaifosi.commands

import bookofaifosi.Bot
import bookofaifosi.model.{DiscordID, User}
import bookofaifosi.model.event.SlashCommandEvent
import cats.data.{EitherT, OptionT}
import cats.effect.IO
import org.typelevel.log4cats.Logger
import doobie.implicits.*

case class Task(
  id: Int,
  tittle: String,
  description: String,
)

object Task extends SlashCommand with Options {
  override val defaultEnabled: Boolean = false
  override val fullCommand: String = "task"
  override val options: List[PatternOption] = List(
    _.addOption[String]("discord_user_id", "Discord id to delete data for."),
    _.addOption[String]("tag", "Tag of the task."),
  )

  override def apply(pattern: SlashPattern, event: SlashCommandEvent)(using Logger[IO]): IO[Boolean] =
    val tag = event.getOption[String]("tag")
    val discordUserID = event.getOption[String]("discord_user_id").toLongOption.map(DiscordID(_))
    (for
      discordUserID <- EitherT.fromOption[IO](discordUserID, "Invalid discord ID")
      task <- OptionT(fr"call GetTask('', $tag)".query[Task].option.transact(Bot.mysqlTransactor)).toRight("Failed to get task!")
      message <- EitherT.liftF(event.author.sendMessage(s"Rolled task ${task.tittle}"))
      _ <- EitherT.liftF(message.addReaction("✅"))
    yield task).foldF(
      error => event.replyEphemeral(error),
      task => event.replyEphemeral(task.toString),
    )
      .as(true)

  override val description: String = "Gets a random task."
}
