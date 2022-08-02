package bookofaifosi.db

import bookofaifosi.Bot
import bookofaifosi.db.mkFragment
import bookofaifosi.db.Filters.*
import bookofaifosi.model.{ChasterID, DiscordID, RegisteredUser}
import cats.data.OptionT
import cats.effect.IO
import doobie.{ConnectionIO, Fragment}
import doobie.postgres.implicits.*
import doobie.syntax.string.*
import doobie.syntax.connectionio.*
import doobie.util.Read
import doobie.util.meta.Meta
import cats.syntax.functor.*
import cats.syntax.option.*
import doobie.syntax.SqlInterpolator.SingleFragment

import java.time.Instant
import java.util.UUID

case class User(
  id: UUID,
  chasterName: String,
  discordID: DiscordID,
  keyholderIDs: List[ChasterID],
  isLocked: Boolean,
  isWearer: Boolean,
  isKeyholder: Boolean,
  tokenID: UUID,
  lastLocked: Option[Instant],
  lastKeyheld: Option[Instant],
)

object RegisteredUserRepository extends ModelRepository[User, RegisteredUser]:
  override protected val table: Fragment = fr"users"
  override protected val columns: List[String] = List("id", "chaster_name", "user_discord_id", "keyholder_ids", "is_locked", "is_wearer", "is_keyholder", "token_id", "last_locked", "last_keyheld")

  override def toModel(user: User): IO[RegisteredUser] =
    for
      discord <- Bot.discord.get
      discordUser <- discord.userByID(user.discordID)
      token <- UserTokenRepository.get(user.tokenID.equalID)
    yield new RegisteredUser(user, discordUser, token)

  def add(
    chasterName: String,
    discordID: DiscordID,
    keyholderIDs: List[ChasterID],
    isLocked: Boolean,
    isWearer: Boolean,
    isKeyholder: Boolean,
    tokenID: UUID,
  ): IO[RegisteredUser] =
    sql"insert into $table (chaster_name, user_discord_id, keyholder_ids, is_locked, is_wearer, is_keyholder, token_id) values ($chasterName, $discordID, $keyholderIDs, $isLocked, $isWearer, $isKeyholder, $tokenID)"
      .update
      .withUniqueGeneratedKeys[User](columns*)
      .transact(Bot.xa)
      .flatMap(toModel)

  def update(
    id: UUID,
    keyholderIDs: Option[List[ChasterID]] = None,
    isLocked: Option[Boolean] = None,
    isWearer: Option[Boolean] = None,
    isKeyholder: Option[Boolean] = None,
    tokenID: Option[UUID] = None,
    lastLocked: Option[Option[Instant]] = None,
    lastKeyheld: Option[Option[Instant]] = None,
  ): IO[RegisteredUser] =
    update(
      keyholderIDs.map(keyholderIDs => fr"keyholder_ids = $keyholderIDs"),
      isLocked.map(isLocked => fr"is_locked = $isLocked"),
      isWearer.map(isWearer => fr"is_wearer = $isWearer"),
      isKeyholder.map(isKeyholder => fr"is_keyholder = $isKeyholder"),
      tokenID.map(tokenID => fr"token_id = $tokenID"),
      lastLocked.map(lastLocked => fr"last_locked = $lastLocked"),
      lastKeyheld.map(lastKeyheld => fr"last_keyheld = $lastKeyheld"),
    )(fr"id = $id")
