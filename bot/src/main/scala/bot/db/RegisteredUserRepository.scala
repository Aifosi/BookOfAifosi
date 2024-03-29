package bot.db

import bot.Bot
import bot.chaster.ChasterClient
import bot.db.{mkFragment, ModelRepository, User, UserTokenRepository}
import bot.db.Filters.*
import bot.db.mkFragment
import bot.model.{ChasterID, Discord, DiscordID, RegisteredUser}
import bot.model.given
import bot.syntax.io.*
import bot.utils.Maybe

import cats.data.EitherT
import cats.effect.{Deferred, IO}
import cats.effect.LiftIO.*
import cats.syntax.functor.*
import cats.syntax.option.*
import doobie.{ConnectionIO, Fragment, LogHandler, Transactor}
import doobie.postgres.implicits.*
import doobie.syntax.SqlInterpolator.SingleFragment
import doobie.syntax.connectionio.*
import doobie.syntax.string.*
import java.time.Instant
import java.util.UUID

case class User(
  id: UUID,
  chasterID: ChasterID,
  discordID: DiscordID,
  guildID: DiscordID,
  keyholderIDs: List[ChasterID],
  isLocked: Boolean,
  tokenID: UUID,
  lastLocked: Option[Instant],
  lastKeyheld: Option[Instant],
)

class RegisteredUserRepository(
  discord: Deferred[IO, Discord],
  userTokenRepository: UserTokenRepository,
  chasterClient: ChasterClient,
)(using
  transactor: Transactor[IO],
  logHandler: LogHandler,
) extends ModelRepository[User, RegisteredUser] with ThoroughList[User, RegisteredUser, String]:
  override protected val table: Fragment       = fr"users"
  override protected val columns: List[String] = List(
    "id",
    "chaster_id",
    "user_discord_id",
    "guild_discord_id",
    "keyholder_ids",
    "is_locked",
    "token_id",
    "last_locked",
    "last_keyheld",
  )

  override def id(db: User): String =
    s"guild ID: ${db.guildID}, discord ID: ${db.discordID}, chaster ID: ${db.chasterID}"

  override def toModel(user: User): Maybe[RegisteredUser] =
    for
      discord <- discord.get.to[Maybe]
      guild   <- discord.guildByID(user.guildID)
      member  <- guild.member(user.discordID)
    yield new RegisteredUser(
      user.id,
      user.chasterID,
      user.guildID,
      user.keyholderIDs,
      user.isLocked,
      user.lastLocked,
      user.lastKeyheld,
      member,
      userTokenRepository.get(user.tokenID.equalID).flatMap(chasterClient.updatedToken),
      user.tokenID,
    )

  def add(
    chasterID: ChasterID,
    discordID: DiscordID,
    guildID: DiscordID,
    keyholderIDs: List[ChasterID],
    isLocked: Boolean,
    tokenID: UUID,
  ): IO[RegisteredUser] =
    sql"insert into $table (chaster_id, user_discord_id, guild_discord_id, keyholder_ids, is_locked, token_id) values ($chasterID, $discordID, $guildID, $keyholderIDs, $isLocked, $tokenID)".update
      .withUniqueGeneratedKeys[User](columns*)
      .transact(transactor)
      .flatMap(unsafeToModel)

  def update(
    id: UUID,
    keyholderIDs: Option[List[ChasterID]] = None,
    isLocked: Option[Boolean] = None,
    tokenID: Option[UUID] = None,
    lastLocked: Option[Option[Instant]] = None,
    lastKeyheld: Option[Option[Instant]] = None,
  ): IO[RegisteredUser] =
    update(
      keyholderIDs.map(keyholderIDs => fr"keyholder_ids = $keyholderIDs"),
      isLocked.map(isLocked => fr"is_locked = $isLocked"),
      tokenID.map(tokenID => fr"token_id = $tokenID"),
      lastLocked.map(lastLocked => fr"last_locked = $lastLocked"),
      lastKeyheld.map(lastKeyheld => fr"last_keyheld = $lastKeyheld"),
    )(fr"id = $id")
