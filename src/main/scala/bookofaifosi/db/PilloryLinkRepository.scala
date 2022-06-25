package bookofaifosi.db

import bookofaifosi.Bot
import bookofaifosi.model.{DiscordID, PilloryLink as PilloryLinkModel}
import bookofaifosi.db.Filters.*
import cats.effect.IO
import doobie.syntax.string.*
import doobie.postgres.implicits.*
import cats.syntax.functor.*
import cats.syntax.traverse.*
import doobie.syntax.connectionio.*
import doobie.util.log.LogHandler
import doobie.Fragment

import java.time.Instant
import java.util.UUID

case class PilloryLink(
  userID: UUID,
  guildID: DiscordID,
  postID: String,
  counted: Boolean,
)

object PilloryLinkRepository extends ModelRepository[PilloryLink, PilloryLinkModel]:
  override protected val table: Fragment = fr"pillory_links"
  override protected val selectColumns: Fragment = fr"user_id, guild_discord_id, post_id, counted"

  override def toModel(pilloryLink: PilloryLink): IO[PilloryLinkModel] =
    for
      user <- RegisteredUserRepository.get(pilloryLink.userID.equalID)
      discord <- Bot.discord.get
      guild <- discord.guildByID(pilloryLink.guildID)
    yield PilloryLinkModel(user, guild, pilloryLink.postID, pilloryLink.counted)

  def add(
    userID: UUID,
    guildID: DiscordID,
    postID: String,
  ): IO[PilloryLinkModel] =
    sql"insert into pillory_links(user_id, guild_discord_id, post_id) values ($userID, $guildID, $postID)"
      .update
      .withUniqueGeneratedKeys[PilloryLink]("user_id", "guild_discord_id", "post_id", "counted")
      .transact(Bot.xa)
      .flatMap(toModel)

  def setCounted(
    guildID: DiscordID,
  ): IO[Unit] =
    sql"update pillory_links set counted = TRUE where guild_discord_id = $guildID"
      .update
      .run
      .void
      .transact(Bot.xa)
