package bot.commands

import bot.Bot
import bot.commands.{Options, PatternOption, SlashCommand, SlashPattern, SlowResponse}
import bot.db.Filters.*
import bot.db.{*, given}
import bot.model.event.{SlashAPI, SlashCommandEvent}
import bot.model.{DiscordID, User}
import cats.data.{EitherT, OptionT}
import cats.effect.{IO, Ref}
import doobie.implicits.*
import doobie.util.transactor.Transactor
import bot.LurchLogger
import bot.wheel.Task as WheelTask
import org.typelevel.log4cats.Logger

case class Task(
  id: Int,
  title: String,
  description: String,
):
  def cleanedDescription: Task = copy(description = description.replaceAll("<br/?>", "\n").replaceAll("<.+?>", ""))

class TaskCommand(
  registeredUserRepository: RegisteredUserRepository,
  mySqlTransactor: Transactor[IO],
)(using LurchLogger) extends SlashCommand with Options with SlowResponse:
  override val isUserCommand: Boolean = false
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
      user <- registeredUserRepository.find(discordUser.discordID.equalDiscordID).toRight(s"Couldn't find registered user $discordUser")
      task <- WheelTask.handleTask(tag, user, mySqlTransactor).toRight("Failed to get task.")
    yield task
    eitherTResponse(response, slashAPI).void

  override val description: String = "Gets a random task for a user."
