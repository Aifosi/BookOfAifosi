package bookofaifosi.db

import bookofaifosi.Bot
import cats.syntax.option.*
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
  override protected val columns: List[String] = List("id", "access_token", "expires_at", "refresh_token", "scope")

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
    update(
      fr"access_token = $accessToken".some,
      fr"expires_at = $expiresAt".some,
      fr"refresh_token = $refreshToken".some,
      fr"scope = $scope".some,
    )(fr"id = $id")