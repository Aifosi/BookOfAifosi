package lurch.commands

import bot.Bot
import bot.commands.{Options, PatternOption, SlashCommand, SlashPattern, SlowResponse}
import bot.db.Filters.*
import bot.db.{RegisteredUserRepository, *, given}
import bot.model.event.{SlashAPI, SlashCommandEvent}
import bot.model.{DiscordID, User}
import cats.data.{EitherT, OptionT}
import cats.effect.{IO, Ref}
import doobie.implicits.*
import lurch.tasks.WheelTasks
import org.typelevel.log4cats.Logger

case class Task(
  id: Int,
  title: String,
  description: String,
):
  def cleanedDescription: Task = copy(description = description.replaceAll("<br/?>", "\n").replaceAll("<.+?>", ""))

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
    eitherTResponse(response, slashAPI).void

  override val description: String = "Gets a random task for a user."
