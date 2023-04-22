package bot.db

import bot.Bot
import bot.db.Filters.*
import bot.model.UserToken
import bot.utils.Maybe

import cats.data.EitherT
import cats.effect.IO
import cats.syntax.option.*
import doobie.{ConnectionIO, Fragment, LogHandler, Transactor}
import doobie.postgres.implicits.*
import doobie.syntax.connectionio.*
import doobie.syntax.string.*
import java.time.Instant
import java.util.UUID

class UserTokenRepository(using transactor: Transactor[IO], logHandler: LogHandler)
    extends ModelRepository[UserToken, UserToken] with RepositoryFields:
  override protected val table: Fragment       = fr"user_tokens"
  override protected val columns: List[String] = List("id", "access_token", "expires_at", "refresh_token", "scope")

  override def toModel(userToken: UserToken): Maybe[UserToken] = EitherT.pure(userToken)

  def add(
    accessToken: String,
    expiresAt: Instant,
    refreshToken: String,
    scope: String,
  ): IO[UserToken] =
    sql"insert into $table (access_token, expires_at, refresh_token, scope) values ($accessToken, $expiresAt, $refreshToken, $scope)".update
      .withUniqueGeneratedKeys[UserToken]("id", "access_token", "expires_at", "refresh_token", "scope")
      .transact(transactor)

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
