package bookofaifosi.commands

import bookofaifosi.db.RegisteredUserRepository
import bookofaifosi.db.Filters.*
import bookofaifosi.model.{Channel, User, DiscordID, ChasterID}
import bookofaifosi.model.event.SlashCommandEvent
import cats.effect.IO
import cats.data.EitherT
import org.typelevel.log4cats.Logger

object Nuke extends SlashCommand with Options:
  override val defaultEnabled: Boolean = false
  override val fullCommand: String = "nuke"
  override val options: List[PatternOption] = List(
    _.addOption[Option[User]]("user", "Discord user mention to delete data for."),
    _.addOption[Option[Long]]("discord_user_id", "Discord id to delete data for."),
    _.addOption[Option[String]]("chaster_user_id", "Chaster id to delete data for."),
  )

  override def apply(pattern: SlashPattern, event: SlashCommandEvent)(using Logger[IO]): IO[Boolean] =
    val user = event.getOption[Option[User]]("user")
    val discordUserID = event.getOption[Option[Long]]("discord_user_id").map(DiscordID(_))
    val chasterUserID = event.getOption[Option[String]]("chaster_user_id").map(ChasterID(_))

    (for
      _ <- EitherT.cond[IO](List(user, discordUserID, chasterUserID).count(_.isDefined) == 1, (), "Please specify exactly 1 way to identify a user.")
      discordDeletes <- EitherT.liftF(user.map(_.discordID).orElse(discordUserID).fold(IO.pure(0))(discordID => RegisteredUserRepository.remove(discordID.equalDiscordID)))
      chasterDeletes <- EitherT.liftF(chasterUserID.fold(IO.pure(0))(chasterID => RegisteredUserRepository.remove(chasterID.equalChasterID)))
      _ <- EitherT.cond(discordDeletes + chasterDeletes == 1, (), "Could not find user to delete!")
    yield ()).foldF(
      error => event.replyEphemeral(error),
      _ => event.replyEphemeral("User data deleted"),
    )
      .as(true)

  override val description: String = "Deletes data for the given user"
