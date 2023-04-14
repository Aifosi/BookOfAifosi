package bot.db

import bot.Bot
import bot.db.Filters.*
import bot.db.given
import bot.db.ModelRepository
import bot.model.{Discord, DiscordID}
import bot.utils.Maybe
import cats.data.EitherT
import cats.effect.{Deferred, IO}
import cats.effect.LiftIO.*
import cats.syntax.applicative.*
import cats.syntax.functor.*
import cats.syntax.option.*
import doobie.{Fragment, LogHandler, Transactor}
import doobie.syntax.connectionio.*
import doobie.syntax.string.*
import bot.model.PilloryBitches as PilloryBitchesModel

case class PilloryBitches(
  guildID: DiscordID,
  channelID: DiscordID,
)

class PilloryBitchesRepository(
  discord: Deferred[IO, Discord],
)(
  using transactor: Transactor[IO], logHandler: LogHandler
) extends ModelRepository[PilloryBitches, PilloryBitchesModel]:
  override protected val table: Fragment = fr"pillory_bitches"
  override protected val columns: List[String] = List("guild_discord_id", "channel_discord_id")

  override def toModel(pilloryBitches: PilloryBitches): Maybe[PilloryBitchesModel] =
    for
      discord <- discord.get.to[Maybe]
      guild <- discord.guildByID(pilloryBitches.guildID)
      channel <- discord.channelByID(pilloryBitches.channelID)
    yield PilloryBitchesModel(guild, channel)

  def addOrUpdate(
    guildID: DiscordID,
    channelID: DiscordID,
  ): IO[PilloryBitchesModel] =
    add(guildID, channelID).attempt.flatMap {
      _.fold(
        throwable => update(guildID, channelID),
        _.pure
      )
    }

  def add(
    guildID: DiscordID,
    channelID: DiscordID,
  ): IO[PilloryBitchesModel] =
    sql"insert into $table (guild_discord_id, channel_discord_id) values ($guildID, $channelID)"
      .update
      .withUniqueGeneratedKeys[PilloryBitches]("guild_discord_id", "channel_discord_id")
      .transact(transactor)
      .flatMap(unsafeToModel)

  def update(
    guildID: DiscordID,
    newChannelID: DiscordID,
  ): IO[PilloryBitchesModel] =
    update(
      fr"channel_discord_id = $newChannelID".some,
    )(fr"guild_discord_id = $guildID")
