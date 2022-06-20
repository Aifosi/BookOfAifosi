package bookofaifosi.db

import bookofaifosi.Bot
import bookofaifosi.chaster.Client
import cats.effect.IO
import doobie.{ConnectionIO, Fragment}
import doobie.syntax.string.*
import doobie.postgres.implicits.*
import cats.syntax.functor.*
import doobie.util.log.LogHandler
import org.http4s.client.Client as HTTPClient

import java.time.OffsetDateTime

case class User(
  chasterName: String,
  discordID: Long,
  accessToken: String,
  expiresAt: OffsetDateTime,
  refreshToken: String,
  scope: String,
):
  def client(client: HTTPClient[IO]) = Client(client, accessToken, expiresAt, refreshToken)

object User extends Queries[User]:
  override protected val selectAll: Fragment = fr"select chaster_name, discord_id, access_token, expires_at, refresh_token, scope from users"

  def add(
    chasterName: String,
    discordID: Long,
    accessToken: String,
    expiresAt: OffsetDateTime,
    refreshToken: String,
    scope: String,
  ): ConnectionIO[Unit] =
    sql"insert into users(chaster_name, discord_id, access_token, expires_at, refresh_token, scope) values ($chasterName, $discordID, $accessToken, $expiresAt, $refreshToken, $scope)".updateWithLogHandler(LogHandler.jdkLogHandler).run.void
