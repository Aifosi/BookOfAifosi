package bot.db

import bot.Bot
import bot.db.Filters.*
import bot.model.{ChasterID, DiscordID, toLong}
import bot.utils.Maybe
import doobie.*
import doobie.implicits.*
import doobie.postgres.*
import doobie.postgres.implicits.*
import doobie.util.{Read, Write}
import doobie.util.log.LogHandler
import cats.syntax.option.*
import cats.syntax.traverse.*

import java.util.UUID
import scala.concurrent.duration.*
import scala.util.chaining.*
import cats.arrow.FunctionK
import cats.data.{EitherT, OptionT}
import cats.effect.IO
import fs2.Stream

given Get[FiniteDuration] = Get[Long].map(_.seconds)
given Put[FiniteDuration] = Put[Long].contramap(_.toSeconds)

given Get[DiscordID] = Get[Long].map(DiscordID(_))
given Put[DiscordID] = Put[Long].contramap(_.toLong)

type Filter = Option[Fragment]

lazy val translator: FunctionK[ConnectionIO, IO] = new FunctionK[ConnectionIO, IO]:
  override def apply[A](fa: ConnectionIO[A]): IO[A] = fa.transact(Bot.postgres.transactor)

extension (filters: List[Filter])
  def mkFragment(start: Fragment = Fragment.empty, sep: Fragment, end: Fragment = Fragment.empty): Fragment =
    val flattened = filters.flatten
    if flattened.isEmpty then
      Fragment.empty
    else
      start ++ flattened.reduceLeft(_ ++ sep ++ _) ++ end
  def combineFilters: Fragment = mkFragment(fr"where", fr"and")

object Filters:
  extension (id: UUID)
    def equalID: Filter = fr"id = $id".some
    def equalUserID: Filter = fr"user_id = $id".some

  extension (id: Option[UUID])
    def equalID: Filter = id.flatMap(_.equalID)

  extension (id: DiscordID)
    def equalDiscordID: Filter = fr"user_discord_id = $id".some
    def equalGuildID: Filter = fr"guild_discord_id = $id".some
    def equalRoleID: Filter = fr"role_discord_id = $id".some
    def equalChannelID: Filter = fr"channel_discord_id = $id".some
    def equalMessageID: Filter = fr"message_discord_id = $id".some

  extension (id: ChasterID)
    def equalChasterID: Filter = fr"chaster_id = $id".some
    def equalLockID: Filter = fr"lock_id = $id".some

  extension (string: String)
    def similarName: Filter = fr"name ILIKE $string".some
    def similarPartialName: Filter = fr"name ILIKE ${s"%$string%"}".some
    def equalName: Filter = fr"name = $string".some
    def equalUserType: Filter = fr"user_type = $string".some
    def equalAccessToken: Filter = fr"access_token = $string".some

  extension (name: Option[String])
    def similarName: Filter = name.flatMap(_.similarName)
    def similarPartialName: Filter = name.flatMap(_.similarPartialName)
    def equalName: Filter = name.flatMap(_.equalName)

  extension (keyholders: List[ChasterID])
    def anyKeyholder: Filter = fr"chaster_id = ANY ($keyholders)".some

  def descriptionEqual(description: Option[Option[String]]): Filter = description.map(description => fr"description = ${
    description
      .orNull
  }",
  )

trait Remove:
  protected val table: Fragment

  def remove(filter: Filter, moreFilters: Filter*): IO[Int] =
    (fr"delete from" ++ table ++ (filter +: moreFilters).toList.combineFilters ++ fr"")
      .update
      .run
      .transact(Bot.postgres.transactor)

trait Insert[DB: Read]:
  outer: RepositoryFields =>
  protected val table: Fragment

  protected def unknowns(columns: Int): String = List.fill(columns)("?").mkString("(", ", ", ")")

  protected def sql(columns: String*): String = fr"insert into $table"
    .internals
    .sql + columns.mkString("(", ", ", ") values") + unknowns(columns.length)

  def insertOne[Info: Write](info: Info)(columns: String*): ConnectionIO[DB] =
    Update[Info](sql(columns *)).withUniqueGeneratedKeys[DB](outer.columns *)(info)

  def insertMany[Info: Write](info: List[Info])(columns: String*): Stream[ConnectionIO, DB] =
    Update[Info](sql(columns *)).updateManyWithGeneratedKeys[DB](outer.columns *)(info)

sealed trait RepositoryFields:
  protected val table: Fragment
  protected val columns: List[String]

trait Repository[DB: Read] extends RepositoryFields with Remove:
  private val updatedAt: Filter = fr"updated_at = NOW()".some

  inline private def updateQuery(updates: Filter*)(where: Fragment, more: Fragment*) =
    (updates.toList :+ updatedAt)
      .mkFragment(fr"update $table set", fr",", (where +: more.toList).map(_.some).combineFilters)
      .update

  inline protected def innerUpdateMany(updates: Filter*)(where: Fragment, more: Fragment*): IO[List[DB]] =
    updateQuery(updates*)(where, more*)
      .withGeneratedKeys[DB](columns*)
      .compile
      .toList
      .transact(Bot.postgres.transactor)

  inline protected def innerUpdate(updates: Filter*)(where: Fragment, more: Fragment*): IO[DB] =
    updateQuery(updates*)(where, more*)
      .withUniqueGeneratedKeys[DB](columns*)
      .transact(Bot.postgres.transactor)

  protected lazy val selectAll: Fragment = Fragment.const(columns.mkString("select ", ", ", " from")) ++ table

  private def query(filters: Iterable[Filter]) =
    (selectAll ++ filters.toList.combineFilters).query[DB]

  def list(filters: Filter*): IO[List[DB]] = query(filters).to[List].transact(Bot.postgres.transactor)

  def find(filters: Filter*): OptionT[IO, DB] = OptionT(query(filters).option.transact(Bot.postgres.transactor))

  def get(filters: Filter*): IO[DB] = find(filters *)
    .value
    .map(_.toRight(new Exception(s"Failed to find item in repository")))
    .rethrow

  def update(updates: Filter*)(where: Fragment, more: Fragment*): IO[DB] = innerUpdate(updates *)(where, more *)

  def updateMany(updates: Filter*)(where: Fragment, more: Fragment*): IO[List[DB]] =
    innerUpdateMany(updates *)(where, more *)

trait ModelRepository[DB: Read, Model] extends RepositoryFields with Remove:
  outer =>
  def toModel(a: DB): Maybe[Model]

  final def unsafeToModel(a: DB): IO[Model] = toModel(a).rethrowT

  private[db] object Repo extends Repository[DB]:
    override protected val table: Fragment = outer.table
    override protected val columns: List[String] = outer.columns

  inline private def toModelList(dbModel: DB): IO[List[Model]] = toModel(dbModel).value.map(_.toSeq.toList)

  def list(filters: Filter*): IO[List[Model]] = Repo.list(filters *).flatMap(_.flatTraverse(toModelList(_)))

  def find(filters: Filter*): OptionT[IO, Model] = Repo.find(filters *).flatMap(toModel(_).toOption)

  def get(filters: Filter*): IO[Model] = Repo.get(filters *).flatMap(unsafeToModel)

  def update(updates: Filter*)(where: Fragment, more: Fragment*): IO[Model] =
    Repo.update(updates *)(where, more *).flatMap(unsafeToModel)

  def updateMany(updates: Filter*)(where: Fragment, more: Fragment*): IO[List[Model]] =
    Repo.updateMany(updates *)(where, more *).flatMap(_.traverse(unsafeToModel))

trait ThoroughList[DB: Read, Model, ID] extends ModelRepository[DB, Model]:
  def id(db: DB): ID
  def thoroughList(filters: Filter*): IO[List[Either[ID, Model]]] =
    Repo.list(filters *).flatMap(_.traverse(dbModel => toModel(dbModel).leftMap(_ => id(dbModel)).value))
