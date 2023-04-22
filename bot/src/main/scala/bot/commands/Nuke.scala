package bot.commands

import bot.db.{RegisteredUserRepository, UserTokenRepository}
import bot.db.Filters.*
import bot.model.{Channel, ChasterID, DiscordID, RegisteredUser, User}
import bot.model.event.SlashCommandEvent

import cats.data.{EitherT, OptionT}
import cats.effect.IO
import cats.syntax.traverse.*
import org.typelevel.log4cats.Logger

class Nuke(
  registeredUserRepository: RegisteredUserRepository,
  userTokenRepository: UserTokenRepository,
) extends SlashCommand with Options:
  override val isUserCommand: Boolean       = false
  override val fullCommand: String          = "nuke"
  override val options: List[PatternOption] = List(
    _.addOption[Option[User]]("user", "Discord user mention to delete data for."),
    _.addOption[Option[String]]("discord_user_id", "Discord id to delete data for."),
    _.addOption[Option[String]]("chaster_user_id", "Chaster id to delete data for."),
  )

  private def userFromDiscordUserID(
    user: Option[User],
    discordUserID: Option[DiscordID],
    guildID: DiscordID,
  ): OptionT[IO, RegisteredUser] =
    OptionT
      .fromOption(user.map(_.discordID).orElse(discordUserID))
      .flatMap(discordID => registeredUserRepository.find(discordID.equalDiscordID, guildID.equalGuildID))

  private def userFromChasterUserID(chasterUserID: Option[ChasterID], guildID: DiscordID): OptionT[IO, RegisteredUser] =
    OptionT
      .fromOption(chasterUserID)
      .flatMap(chasterID => registeredUserRepository.find(chasterID.equalChasterID, guildID.equalGuildID))

  override def apply(pattern: SlashPattern, event: SlashCommandEvent)(using Logger[IO]): IO[Boolean] =
    val user          = event.getOption[Option[User]]("user")
    val discordUserID = event.getOption[Option[String]]("discord_user_id").flatMap(_.toLongOption).map(DiscordID(_))
    val chasterUserID = event.getOption[Option[String]]("chaster_user_id").map(ChasterID(_))

    (for
      _     <- EitherT.cond[IO](
                 List(user, discordUserID, chasterUserID).count(_.isDefined) == 1,
                 (),
                 "Please specify exactly 1 way to identify a user.",
               )
      guild <- EitherT.liftF(event.guild)
      user  <- userFromDiscordUserID(user, discordUserID, guild.discordID)
                 .orElse(userFromChasterUserID(chasterUserID, guild.discordID))
                 .toRight("Could not find user to delete!")
      _     <- EitherT.liftF(userTokenRepository.remove(user.tokenID.equalID))
      _     <- EitherT.liftF(registeredUserRepository.remove(user.id.equalID))
    yield ())
      .foldF(
        error => event.replyEphemeral(error),
        _ => event.replyEphemeral("User data deleted"),
      )
      .as(true)

  override val description: String = "Deletes data for the given user"
