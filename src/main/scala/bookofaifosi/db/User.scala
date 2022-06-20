package bookofaifosi.db

import bookofaifosi.Bot
import bookofaifosi.chaster.Client
import bookofaifosi.db.mkFragment
import cats.effect.IO
import doobie.{ConnectionIO, Fragment}
import doobie.syntax.string.*
import doobie.postgres.implicits.*
import cats.syntax.functor.*
import doobie.util.log.LogHandler
import org.http4s.client.Client as HTTPClient

import java.time.Instant

case class User(
  chasterName: String,
  discordID: Long,
  accessToken: String,
  expiresAt: Instant,
  refreshToken: String,
  scope: String,
)

object User:
  private val selectAll: Fragment = fr"select chaster_name, discord_id, access_token, expires_at, refresh_token, scope from users"

  def find(chasterName: Option[String] = None, discordID: Option[Long] = None): ConnectionIO[Option[User]] =
    val filterName = chasterName.map(name => fr"chaster_name ILIKE $name")
    val filterId = discordID.map(id => fr"discord_id = $id")
    (selectAll ++ List(filterName, filterId).mkFragment(fr"where", fr"and")).queryWithLogHandler[User](LogHandler.jdkLogHandler).option

  def add(
    chasterName: String,
    discordID: Long,
    accessToken: String,
    expiresAt: Instant,
    refreshToken: String,
    scope: String,
  ): ConnectionIO[User] =
    sql"insert into users(chaster_name, discord_id, access_token, expires_at, refresh_token, scope) values ($chasterName, $discordID, $accessToken, $expiresAt, $refreshToken, $scope)"
      .updateWithLogHandler(LogHandler.jdkLogHandler)
      .withUniqueGeneratedKeys("chaster_name", "discord_id", "access_token", "expires_at", "refresh_token", "scope")

  def update(
    chasterName: String,
    discordID: Long,
    accessToken: String,
    expiresAt: Instant,
    refreshToken: String,
    scope: String,
  ): ConnectionIO[User] =
    sql"update users set access_token = $accessToken, expires_at = $expiresAt, refresh_token = $refreshToken, scope = $scope where chaster_name = $chasterName and discord_id = $discordID"
      .updateWithLogHandler(LogHandler.jdkLogHandler)
      .withUniqueGeneratedKeys("chaster_name", "discord_id", "access_token", "expires_at", "refresh_token", "scope")
