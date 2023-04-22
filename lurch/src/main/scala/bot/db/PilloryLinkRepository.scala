package bot.db

import bot.Bot
import bot.db.{ModelRepository, RegisteredUserRepository}
import bot.db.Filters.*
import bot.db.given
import bot.model.{ChasterID, Discord, DiscordID, given}
import bot.model.PilloryLink as PilloryLinkModel
import bot.utils.Maybe

import cats.data.EitherT
import cats.effect.{Deferred, IO}
import cats.effect.LiftIO.*
import cats.syntax.functor.*
import cats.syntax.option.*
import cats.syntax.traverse.*
import doobie.{Fragment, LogHandler, Transactor}
import doobie.postgres.implicits.*
import doobie.syntax.connectionio.*
import doobie.syntax.string.*
import doobie.util.log.LogHandler
import java.time.Instant
import java.util.UUID

case class PilloryLink(
  userID: UUID,
  guildID: DiscordID,
  postID: ChasterID,
  counted: Boolean,
)

class PilloryLinkRepository(
  discord: Deferred[IO, Discord],
  registeredUserRepository: RegisteredUserRepository,
)(using
  transactor: Transactor[IO],
  logHandler: LogHandler,
) extends ModelRepository[PilloryLink, PilloryLinkModel]:
  override protected val table: Fragment       = fr"pillory_links"
  override protected val columns: List[String] = List("user_id", "guild_discord_id", "post_id", "counted")

  override def toModel(pilloryLink: PilloryLink): Maybe[PilloryLinkModel] =
    for
      user    <- registeredUserRepository.get(pilloryLink.userID.equalID).to[Maybe]
      discord <- discord.get.to[Maybe]
      guild   <- discord.guildByID(pilloryLink.guildID)
    yield PilloryLinkModel(user, guild, pilloryLink.postID, pilloryLink.counted)

  def add(
    userID: UUID,
    guildID: DiscordID,
    postID: ChasterID,
  ): IO[PilloryLinkModel] =
    sql"insert into $table (user_id, guild_discord_id, post_id) values ($userID, $guildID, $postID)".update
      .withUniqueGeneratedKeys[PilloryLink]("user_id", "guild_discord_id", "post_id", "counted")
      .transact(transactor)
      .flatMap(unsafeToModel)

  def setCounted(
    guildID: DiscordID,
  ): IO[Unit] =
    updateMany(
      fr"counted = TRUE".some,
    )(fr"guild_discord_id = $guildID").void
