package bookofaifosi.db

import bookofaifosi.Bot
import bookofaifosi.db.mkFragment
import bookofaifosi.model.{DiscordID, RegisteredUser}
import cats.data.OptionT
import cats.effect.IO
import doobie.{ConnectionIO, Fragment}
import doobie.syntax.string.*
import doobie.postgres.implicits.*
import cats.syntax.functor.*
import doobie.util.log.LogHandler
import doobie.syntax.connectionio.*

import java.time.Instant
import java.util.UUID

case class User(
  id: UUID,
  chasterName: String,
  discordID: DiscordID,
  accessToken: String,
  expiresAt: Instant,
  refreshToken: String,
  scope: String,
  isWearer: Boolean,
  isKeyholder: Boolean,
)

object RegisteredUserRepository extends ModelRepository[User, RegisteredUser]:
  override protected val table: Fragment = fr"users"
  override protected val selectColumns: Fragment = fr"id, chaster_name, user_discord_id, access_token, expires_at, refresh_token, scope, is_wearer, is_keyholder"

  override def toModel(user: User): IO[RegisteredUser] =
    for
      discord <- Bot.discord.get
      discordUser <- discord.userByID(user.discordID)
    yield new RegisteredUser(user, discordUser)

  private def isWearer(scope: String) = scope.split(" ").contains("locks")
  private def isKeyholder(scope: String) = scope.split(" ").contains("keyholder")

  def add(
    chasterName: String,
    discordID: DiscordID,
    accessToken: String,
    expiresAt: Instant,
    refreshToken: String,
    scope: String,
  ): IO[RegisteredUser] =
    sql"insert into users(chaster_name, user_discord_id, access_token, expires_at, refresh_token, scope, is_wearer, is_keyholder) values ($chasterName, $discordID, $accessToken, $expiresAt, $refreshToken, $scope, ${isWearer(scope)}, ${isKeyholder(scope)})"
      .update
      .withUniqueGeneratedKeys[User]("id", "chaster_name", "user_discord_id", "access_token", "expires_at", "refresh_token", "scope", "is_wearer", "is_keyholder")
      .transact(Bot.xa)
      .flatMap(toModel)

  def update(
    id: UUID,
    accessToken: String,
    expiresAt: Instant,
    refreshToken: String,
    scope: String,
  ): IO[RegisteredUser] =
    sql"update users set access_token = $accessToken, expires_at = $expiresAt, refresh_token = $refreshToken, scope = $scope, is_wearer = ${isWearer(scope)}, is_keyholder = ${isKeyholder(scope)} where id = $id"
      .update
      .withUniqueGeneratedKeys[User]("id", "chaster_name", "user_discord_id", "access_token", "expires_at", "refresh_token", "scope", "is_wearer", "is_keyholder")
      .transact(Bot.xa)
      .flatMap(toModel)
