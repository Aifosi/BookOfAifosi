package bot.db

import bot.Bot
import bot.db.Filters.*
import bot.db.given
import bot.db.ModelRepository
import bot.model.{DiscordID, Discord}
import bot.utils.Maybe
import cats.data.EitherT
import cats.effect.{Deferred, IO}
import cats.effect.LiftIO.*
import cats.implicits.*
import cats.syntax.applicative.*
import cats.syntax.functor.*
import cats.syntax.option.*
import doobie.{Fragment, LogHandler, Transactor}
import doobie.syntax.connectionio.*
import doobie.syntax.string.*
import bot.model.LockedChannel as LockedChannelModel

case class LockedChannel(
  guildID: DiscordID,
  channelID: DiscordID,
)

class LockedChannelsRepository(
  discord: Deferred[IO, Discord],
)(
  using transactor: Transactor[IO], logHandler: LogHandler
) extends ModelRepository[LockedChannel, LockedChannelModel]:
  override protected val table: Fragment = fr"locked_channels"
  override protected val columns: List[String] = List("guild_discord_id", "channel_discord_id")

  override def toModel(pilloryBitches: LockedChannel): Maybe[LockedChannelModel] =
    for
      discord <- discord.get.to[Maybe]
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
      .transact(transactor)
      .flatMap(unsafeToModel)
