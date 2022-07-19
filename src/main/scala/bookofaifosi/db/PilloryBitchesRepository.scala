package bookofaifosi.db

import bookofaifosi.Bot
import bookofaifosi.model.DiscordID
import bookofaifosi.model.PilloryBitches as PilloryBitchesModel
import bookofaifosi.db.Filters.*
import cats.effect.IO
import doobie.Fragment
import doobie.syntax.string.*
import doobie.syntax.connectionio.*
import cats.syntax.applicative.*
import cats.syntax.functor.*
import cats.syntax.option.*

case class PilloryBitches(
  guildID: DiscordID,
  channelID: DiscordID,
)

object PilloryBitchesRepository extends ModelRepository[PilloryBitches, PilloryBitchesModel]:
  override protected val table: Fragment = fr"pillory_bitches"
  override protected val selectColumns: Fragment = fr"guild_discord_id, channel_discord_id"

  override def toModel(pilloryBitches: PilloryBitches): IO[PilloryBitchesModel] =
    for
      discord <- Bot.discord.get
      guild <- discord.guildByID(pilloryBitches.guildID)
      channel <- discord.channelByID(pilloryBitches.channelID)
    yield PilloryBitchesModel(guild, channel)

  def addOrUpdate(
    guildID: DiscordID,
    channelID: DiscordID,
  ): IO[PilloryBitches] =
    add(guildID, channelID).attempt.flatMap {
      _.fold(
        throwable => update(guildID, channelID),
        _.pure
      )
    }

  def add(
    guildID: DiscordID,
    channelID: DiscordID,
  ): IO[PilloryBitches] =
    sql"insert into $table (guild_discord_id, channel_discord_id) values ($guildID, $channelID)"
      .update
      .withUniqueGeneratedKeys[PilloryBitches]("guild_discord_id", "channel_discord_id")
      .transact(Bot.xa)

  def update(
    guildID: DiscordID,
    newChannelID: DiscordID,
  ): IO[PilloryBitches] =
    sql"update pillory_bitches set channel_discord_id = $newChannelID where guild_discord_id = $guildID"
      .update
      .withUniqueGeneratedKeys[PilloryBitches]("guild_discord_id", "channel_discord_id")
      .transact(Bot.xa)
