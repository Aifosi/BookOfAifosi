package lurch.db

import bot.Bot
import bot.db.Filters.*
import bot.db.given
import bot.db.Log.given
import bot.db.{ModelRepository, RegisteredUserRepository}
import bot.model.{ChasterID, DiscordID}
import cats.effect.IO
import cats.syntax.functor.*
import cats.syntax.option.*
import cats.syntax.traverse.*
import doobie.Fragment
import doobie.postgres.implicits.*
import doobie.syntax.connectionio.*
import doobie.syntax.string.*
import doobie.util.log.LogHandler
import lurch.model.PilloryLink as PilloryLinkModel

import java.time.Instant
import java.util.UUID

case class PilloryLink(
  userID: UUID,
  guildID: DiscordID,
  postID: ChasterID,
  counted: Boolean,
)

object PilloryLinkRepository extends ModelRepository[PilloryLink, PilloryLinkModel]:
  override protected val table: Fragment = fr"pillory_links"
  override protected val columns: List[String] = List("user_id", "guild_discord_id", "post_id", "counted")

  override def toModel(pilloryLink: PilloryLink): IO[PilloryLinkModel] =
    for
      user <- RegisteredUserRepository.get(pilloryLink.userID.equalID)
      discord <- Bot.discord.get
      guild <- discord.guildByID(pilloryLink.guildID)
    yield PilloryLinkModel(user, guild, pilloryLink.postID, pilloryLink.counted)

  def add(
    userID: UUID,
    guildID: DiscordID,
    postID: ChasterID,
  ): IO[PilloryLinkModel] =
    sql"insert into $table (user_id, guild_discord_id, post_id) values ($userID, $guildID, $postID)"
      .update
      .withUniqueGeneratedKeys[PilloryLink]("user_id", "guild_discord_id", "post_id", "counted")
      .transact(Bot.postgresConfig.transactor)
      .flatMap(toModel)

  def setCounted(
    guildID: DiscordID,
  ): IO[Unit] =
    updateMany(
      fr"counted = TRUE".some
    )(fr"guild_discord_id = $guildID")
      .void
