package bookofaifosi.db

import bookofaifosi.Bot
import bookofaifosi.db.mkFragment
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
  discordID: Long,
  accessToken: String,
  expiresAt: Instant,
  refreshToken: String,
  scope: String,
)

object UserRepository:
  private val selectAll: Fragment = fr"select id, chaster_name, discord_id, access_token, expires_at, refresh_token, scope from users"

  def find(id: Option[UUID] = None, chasterName: Option[String] = None, discordID: Option[Long] = None): IO[Option[User]] =
    val filterId = id.map(id => fr"id = $id")
    val filterName = chasterName.map(name => fr"chaster_name ILIKE $name")
    val filterDiscordId = discordID.map(id => fr"discord_id = $id")
    (selectAll ++ List(filterId, filterName, filterDiscordId).mkFragment(fr"where", fr"and"))
      .queryWithLogHandler[User](LogHandler.jdkLogHandler)
      .option
      .transact(Bot.xa)

  def add(
    chasterName: String,
    discordID: Long,
    accessToken: String,
    expiresAt: Instant,
    refreshToken: String,
    scope: String,
  ): IO[User] =
    sql"insert into users(chaster_name, discord_id, access_token, expires_at, refresh_token, scope) values ($chasterName, $discordID, $accessToken, $expiresAt, $refreshToken, $scope)"
      .updateWithLogHandler(LogHandler.jdkLogHandler)
      .withUniqueGeneratedKeys[User]("id", "chaster_name", "discord_id", "access_token", "expires_at", "refresh_token", "scope")
      .transact(Bot.xa)

  def update(
    id: UUID,
    accessToken: String,
    expiresAt: Instant,
    refreshToken: String,
    scope: String,
  ): IO[User] =
    sql"update users set access_token = $accessToken, expires_at = $expiresAt, refresh_token = $refreshToken, scope = $scope where id = $id"
      .updateWithLogHandler(LogHandler.jdkLogHandler)
      .withUniqueGeneratedKeys[User]("id", "chaster_name", "discord_id", "access_token", "expires_at", "refresh_token", "scope")
      .transact(Bot.xa)
