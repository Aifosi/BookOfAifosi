package bookofaifosi.db

import bookofaifosi.Bot
import bookofaifosi.db.mkFragment
import bookofaifosi.db.Filters.*
import bookofaifosi.model.{DiscordID, RegisteredUser}
import cats.data.OptionT
import cats.effect.IO
import doobie.{ConnectionIO, Fragment}
import doobie.syntax.string.*
import doobie.postgres.implicits.*
import doobie.syntax.connectionio.*
import cats.syntax.functor.*
import cats.syntax.option.*

import java.time.Instant
import java.util.UUID

case class User(
  id: UUID,
  chasterName: String,
  discordID: DiscordID,
  keyholderIDs: List[String],
  isLocked: Boolean,
  isWearer: Boolean,
  isKeyholder: Boolean,
  tokenID: UUID,
)

object RegisteredUserRepository extends ModelRepository[User, RegisteredUser]:
  override protected val table: Fragment = fr"users"
  override protected val selectColumns: Fragment = fr"id, chaster_name, user_discord_id, keyholder_ids, is_locked, is_wearer, is_keyholder, token_id"

  override def toModel(user: User): IO[RegisteredUser] =
    for
      discord <- Bot.discord.get
      discordUser <- discord.userByID(user.discordID)
      token <- UserTokenRepository.get(user.tokenID.equalID)
    yield new RegisteredUser(user, discordUser, token)

  def add(
    chasterName: String,
    discordID: DiscordID,
    keyholderIDs: List[String],
    isLocked: Boolean,
    isWearer: Boolean,
    isKeyholder: Boolean,
    tokenID: UUID,
  ): IO[RegisteredUser] =
    sql"insert into $table (chaster_name, user_discord_id, keyholder_ids, is_locked, is_wearer, is_keyholder, token_id) values ($chasterName, $discordID, $keyholderIDs, $isLocked, $isWearer, $isKeyholder, $tokenID)"
      .update
      .withUniqueGeneratedKeys[User]("id", "chaster_name", "user_discord_id", "keyholder_ids", "is_locked", "is_wearer", "is_keyholder", "token_id")
      .transact(Bot.xa)
      .flatMap(toModel)

  def update(
    id: UUID,
    keyholderIDs: List[String],
    isLocked: Boolean,
    isWearer: Boolean,
    isKeyholder: Boolean,
    tokenID: UUID,
  ): IO[RegisteredUser] =
    sql"update $table set keyholder_ids = $keyholderIDs, is_locked = $isLocked, is_wearer = $isWearer, is_keyholder = $isKeyholder, token_id = $tokenID where id = $id"
      .update
      .withUniqueGeneratedKeys[User]("id", "chaster_name", "user_discord_id", "keyholder_ids", "is_locked", "is_wearer", "is_keyholder", "token_id")
      .transact(Bot.xa)
      .flatMap(toModel)
