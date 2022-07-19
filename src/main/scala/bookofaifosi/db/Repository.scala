package bookofaifosi.db

import bookofaifosi.Bot
import bookofaifosi.db.Filters.*
import bookofaifosi.model.DiscordID
import cats.effect.IO
import doobie.{ConnectionIO, Fragment, Get, Put}
import doobie.implicits.*
import doobie.postgres.*
import doobie.postgres.implicits.*
import doobie.util.Read
import doobie.util.log.LogHandler
import cats.syntax.option.*
import cats.syntax.traverse.*

import java.util.UUID
import scala.concurrent.duration.*
import scala.util.chaining.*
import bookofaifosi.model.DiscordID
import bookofaifosi.model.toLong

given Get[FiniteDuration] = Get[Long].map(_.seconds)
given Put[FiniteDuration] = Put[Long].contramap(_.toSeconds)

given Get[DiscordID] = Get[Long].map(DiscordID(_))
given Put[DiscordID] = Put[Long].contramap(_.toLong)

type Filter = Option[Fragment]

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
    def equalKeyholderID: Filter = fr"keyholder_id = $id".some

  extension (id: Option[UUID])
    def equalID: Filter = id.flatMap(_.equalID)

  extension (id: DiscordID)
    def equalDiscordID: Filter = fr"user_discord_id = $id".some
    def equalGuildID: Filter = fr"guild_discord_id = $id".some
    def equalRoleID: Filter = fr"role_discord_id = $id".some
    def equalChannelID: Filter = fr"channel_discord_id = $id".some

  extension (string: String)
    def similarName: Filter = fr"name ILIKE $string".some
    def similarPartialName: Filter = fr"name ILIKE ${s"%$string%"}".some
    def equalName: Filter = fr"name = $string".some
    def equalChasterName: Filter = fr"chaster_name = $string".some
    def equalUserType: Filter = fr"user_type = $string".some
    def equalLockID: Filter = fr"lock_id = $string".some
    def equalAccessToken: Filter = fr"access_token = $string".some

  extension (name: Option[String])
    def similarName: Filter = name.flatMap(_.similarName)
    def similarPartialName: Filter = name.flatMap(_.similarPartialName)
    def equalName: Filter = name.flatMap(_.equalName)

  def descriptionEqual(description: Option[Option[String]]): Filter = description.map(description => fr"description = ${description.orNull}")
  lazy val isWearer: Filter = fr"is_wearer = true".some
  lazy val isKeyholder: Filter = fr"is_keyholder = true".some

trait Remove:
  protected val table: Fragment
  def remove(filter: Filter, moreFilters: Filter*): IO[Int] =
    (fr"delete from" ++ table ++ (filter +: moreFilters).toList.combineFilters)
      .update
      .run
      .transact(Bot.xa)

trait Repository[A: Read] extends Remove:
  protected val table: Fragment
  protected val selectColumns: Fragment
  protected lazy val selectAll = fr"select" ++ selectColumns ++ fr"from" ++ table

  private def query(filters: Iterable[Filter]) =
    (selectAll ++ filters.toList.combineFilters).query[A]

  def list(filters: Filter*): IO[List[A]] = query(filters).to[List].transact(Bot.xa)

  def find(filters: Filter*): IO[Option[A]] = query(filters).option.transact(Bot.xa)

  def get(filters: Filter*): IO[A] = find(filters*).flatMap(a => IO.fromOption(a)(new Exception(s"Failed to find item in repository")))

trait ModelRepository[A: Read, Model] extends Remove:
  outer =>
  protected val table: Fragment
  protected val selectColumns: Fragment
  def toModel(a: A): IO[Model]

  private object Repo extends Repository[A]:
    override protected val table: Fragment = outer.table
    override protected val selectColumns: Fragment = outer.selectColumns

  def list(filters: Filter*): IO[List[Model]] = Repo.list(filters*).flatMap(_.traverse(toModel))

  def find(filters: Filter*): IO[Option[Model]] = Repo.find(filters*).flatMap(_.traverse(toModel))

  def get(filters: Filter*): IO[Model] = Repo.get(filters*).flatMap(toModel)
