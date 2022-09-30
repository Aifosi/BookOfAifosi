package bookofaifosi.db

import bookofaifosi.Bot
import bookofaifosi.model.DiscordID
import bookofaifosi.model.LockedChannel as LockedChannelModel
import bookofaifosi.db.Filters.*
import cats.effect.IO
import doobie.Fragment
import doobie.syntax.string.*
import doobie.syntax.connectionio.*
import cats.syntax.applicative.*
import cats.syntax.functor.*
import cats.syntax.option.*

case class LockedChannel(
  guildID: DiscordID,
  channelID: DiscordID,
)

object LockedChannelsRepository extends ModelRepository[LockedChannel, LockedChannelModel]:
  override protected val table: Fragment = fr"locked_channels"
  override protected val columns: List[String] = List("guild_discord_id", "channel_discord_id")

  override def toModel(pilloryBitches: LockedChannel): IO[LockedChannelModel] =
    for
      discord <- Bot.discord.get
      guild <- discord.guildByID(pilloryBitches.guildID)
      channel <- discord.channelByID(pilloryBitches.channelID)
    yield LockedChannelModel(guild, channel)

  def add(
    guildID: DiscordID,
    channelID: DiscordID,
  ): IO[LockedChannelModel] =
    sql"insert into $table (guild_discord_id, channel_discord_id) values ($guildID, $channelID)"
      .update
      .withUniqueGeneratedKeys[LockedChannel]("guild_discord_id", "channel_discord_id")
      .transact(Bot.postgresTransactor)
      .flatMap(toModel)
