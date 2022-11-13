package bot.commands

import bot.db.{RegisteredUserRepository, UserTokenRepository}
import bot.db.Filters.*
import bot.model.{Channel, ChasterID, DiscordID, User}
import bot.model.event.SlashCommandEvent
import cats.effect.IO
import cats.data.{EitherT, OptionT}
import org.typelevel.log4cats.Logger
import cats.syntax.traverse.*

object Nuke extends SlashCommand with Options:
  override val defaultEnabled: Boolean = false
  override val fullCommand: String = "nuke"
  override val options: List[PatternOption] = List(
    _.addOption[Option[User]]("user", "Discord user mention to delete data for."),
    _.addOption[Option[String]]("discord_user_id", "Discord id to delete data for."),
    _.addOption[Option[String]]("chaster_user_id", "Chaster id to delete data for."),
  )

  override def apply(pattern: SlashPattern, event: SlashCommandEvent)(using Logger[IO]): IO[Boolean] =
    val user = event.getOption[Option[User]]("user")
    val discordUserID = event.getOption[Option[String]]("discord_user_id").flatMap(_.toLongOption).map(DiscordID(_))
    val chasterUserID = event.getOption[Option[String]]("chaster_user_id").map(ChasterID(_))

    (for
      _ <- EitherT.cond[IO](List(user, discordUserID, chasterUserID).count(_.isDefined) == 1, (), "Please specify exactly 1 way to identify a user.")
      user <- OptionT {
        user
          .map(_.discordID)
          .orElse(discordUserID)
          .map(discordID => RegisteredUserRepository.find(discordID.equalDiscordID))
          .orElse(chasterUserID.map(chasterID => RegisteredUserRepository.find(chasterID.equalChasterID)))
          .flatSequence
      }
        .toRight("Could not find user to delete!")
      _ <- EitherT.liftF(UserTokenRepository.remove(user.token.id.equalID))
      _ <- EitherT.liftF(RegisteredUserRepository.remove(user.id.equalID))
    yield ()).foldF(
      error => event.replyEphemeral(error),
      _ => event.replyEphemeral("User data deleted"),
    )
      .as(true)

  override val description: String = "Deletes data for the given user"
