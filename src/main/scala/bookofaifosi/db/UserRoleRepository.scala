package bookofaifosi.db

import bookofaifosi.Bot
import bookofaifosi.model.DiscordID
import bookofaifosi.model.UserRole as UserRoleModel
import bookofaifosi.db.Filters.*
import cats.effect.IO
import doobie.Fragment
import doobie.syntax.string.*
import doobie.syntax.connectionio.*
import cats.syntax.applicative.*
import cats.syntax.functor.*
import cats.syntax.option.*

case class UserRole(
  guildID: DiscordID,
  roleID: DiscordID,
  userType: String,
)

object UserRoleRepository extends ModelRepository[UserRole, UserRoleModel]:
  override protected val selectAll: Fragment = fr"select guild_discord_id, role_discord_id, user_type from user_roles"

  override def toModel(userRole: UserRole): IO[UserRoleModel] =
    for
      discord <- Bot.discord.get
      guild <- discord.guildByID(userRole.guildID)
      role <- discord.roleByID(userRole.roleID)
    yield UserRoleModel(guild, role, userRole.userType)

  def addOrUpdate(
    guildID: DiscordID,
    roleID: DiscordID,
    userType: String,
  ): IO[UserRole] =
    add(guildID, roleID, userType).attempt.flatMap {
      _.fold(
        throwable => find(fr"guild_discord_id = $guildID".some, fr"user_type = $userType".some).flatMap(_.fold(IO.raiseError(throwable)) { userRole =>
          update(guildID, userRole.role.discordID, userType, roleID)
        }),
        _.pure
      )
    }

  def add(
    guildID: DiscordID,
    roleID: DiscordID,
    userType: String,
  ): IO[UserRole] =
    sql"insert into user_roles(guild_discord_id, role_discord_id, user_type) values ($guildID, $roleID, $userType)"
      .updateWithLogHandler(Log.handler)
      .withUniqueGeneratedKeys[UserRole]("guild_discord_id", "role_discord_id", "user_type")
      .transact(Bot.xa)

  def update(
    guildID: DiscordID,
    roleID: DiscordID,
    userType: String,
    newRoleID: DiscordID,
  ): IO[UserRole] =
    sql"update user_roles set role_discord_id = $newRoleID where guild_discord_id = $guildID and role_discord_id = $roleID and user_type = $userType"
      .updateWithLogHandler(Log.handler)
      .withUniqueGeneratedKeys[UserRole]("guild_discord_id", "role_discord_id", "user_type")
      .transact(Bot.xa)

  def remove(
    guildID: DiscordID,
    roleID: DiscordID,
    userType: String,
  ): IO[Unit] =
    sql"delete from user_roles where guild_discord_id = $guildID and role_discord_id = $roleID and user_type = $userType"
      .updateWithLogHandler(Log.handler)
      .run
      .void
      .transact(Bot.xa)