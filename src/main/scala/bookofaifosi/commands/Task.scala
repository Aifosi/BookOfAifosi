package bookofaifosi.commands

import bookofaifosi.Bot
import bookofaifosi.model.{DiscordID, User}
import bookofaifosi.model.event.{SlashAPI, SlashCommandEvent}
import cats.data.{EitherT, OptionT}
import cats.effect.{IO, Ref}
import org.typelevel.log4cats.Logger
import doobie.implicits.*
import bookofaifosi.db.Filters.*
import bookofaifosi.db.*
import bookofaifosi.db.given
import bookofaifosi.tasks.WheelTasks

case class Task(
  id: Int,
  tittle: String,
  description: String,
)

object Task extends SlashCommand with Options with SlowResponse:
  override val defaultEnabled: Boolean = false
  override val fullCommand: String = "task"
  override val options: List[PatternOption] = List(
    _.addOption[User]("user", "Discord user to give the task to."),
    _.addOption[String]("tag", "Tag of the task."),
  )

  override val ephemeralResponses: Boolean = true

  override def slowResponse(pattern: SlashPattern, event: SlashCommandEvent, slashAPI: Ref[IO, SlashAPI])(using Logger[IO]): IO[Unit] =
    val tag = event.getOption[String]("tag")
    val discordUser = event.getOption[User]("user")
    val response = for
      user <- OptionT(RegisteredUserRepository.find(discordUser.discordID.equalDiscordID)).toRight(s"Couldn't find registered user $discordUser")
      task <- WheelTasks.handleTask(tag, user).toRight("Failed to get task.")
    yield task
    eitherTResponse(response, slashAPI)

  override val description: String = "Gets a random task for a user."
