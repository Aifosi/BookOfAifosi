package bookofaifosi.db

import bookofaifosi.Bot
import doobie.{ConnectionIO, Fragment}
import doobie.syntax.string.*
import doobie.postgres.implicits.*
import doobie.syntax.connectionio.*
import bookofaifosi.db.Filters.*

import java.time.Instant
import bookofaifosi.model.UserToken
import cats.effect.IO

import java.util.UUID

object UserTokenRepository extends Repository[UserToken]:
  override protected val table: Fragment = fr"user_tokens"
  override protected val selectColumns: Fragment = fr"id, access_token, expires_at, refresh_token, scope"

  def add(
    accessToken: String,
    expiresAt: Instant,
    refreshToken: String,
    scope: String,
  ): IO[UserToken] =
    sql"insert into $table (access_token, expires_at, refresh_token, scope) values ($accessToken, $expiresAt, $refreshToken, $scope)"
      .update
      .withUniqueGeneratedKeys[UserToken]("id", "access_token", "expires_at", "refresh_token", "scope")
      .transact(Bot.xa)

  def update(
    id: UUID,
    accessToken: String,
    expiresAt: Instant,
    refreshToken: String,
    scope: String,
  ): IO[UserToken] =
    sql"update $table set access_token = $accessToken, expires_at = $expiresAt, refresh_token = $refreshToken, scope = $scope, $updatedAt where id = $id"
      .update
      .withUniqueGeneratedKeys[UserToken]("id", "access_token", "expires_at", "refresh_token", "scope")
      .transact(Bot.xa)