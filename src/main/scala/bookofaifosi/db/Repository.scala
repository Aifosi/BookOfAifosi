package bookofaifosi.db

import bookofaifosi.Bot
import bookofaifosi.db.Filters.*
import cats.effect.IO
import doobie.{ConnectionIO, Fragment}
import doobie.implicits.*
import doobie.postgres.*
import doobie.postgres.implicits.*
import doobie.util.Read
import doobie.util.log.LogHandler
import cats.syntax.option.*
import cats.syntax.traverse.*

import java.util.UUID
import scala.util.chaining.*

type Filter = Option[Fragment]

extension (filters: List[Filter])
  def mkFragment(start: Fragment = Fragment.empty, sep: Fragment, end: Fragment = Fragment.empty): Fragment =
    val flattened = filters.flatten
    if flattened.isEmpty then
      Fragment.empty
    else
      start ++ flattened.reduceLeft(_ ++ sep ++ _) ++ end

object Filters:
  extension (id: UUID)
    def equalID: Filter = fr"id = $id".some

  extension (id: Long)
    def equalID: Filter = fr"discord_id = $id".some

  extension (name: String)
    def similarName: Filter = fr"name ILIKE $name".some
    def similarPartialName: Filter = fr"name ILIKE ${s"%$name%"}".some
    def equalName: Filter = fr"name = $name".some

  extension (name: Option[String])
    def similarName: Filter = name.flatMap(_.similarName)
    def similarPartialName: Filter = name.flatMap(_.similarPartialName)
    def equalName: Filter = name.flatMap(_.equalName)


  def descriptionEqual(description: Option[Option[String]]): Filter = description.map(description => fr"description = ${description.orNull}")

trait Repository[A: Read]:
  protected val selectAll: Fragment

  private def query(filters: Iterable[Filter]) =
    (selectAll ++ filters.toList.mkFragment(fr"where", fr"and")).queryWithLogHandler[A](LogHandler.jdkLogHandler)

  def list(filters: Filter*): IO[List[A]] = query(filters).to[List].transact(Bot.xa)

  def find(filters: Filter*): IO[Option[A]] = query(filters).option.transact(Bot.xa)

  def get(filters: Filter*): IO[A] = find(filters*).flatMap(a => IO.fromOption(a)(new Exception(s"Failed to find item in repository")))

trait ModelRepository[A: Read, Model]:
  outer =>
  protected val selectAll: Fragment
  def toModel(a: A): IO[Model]

  private object Repo extends Repository[A]:
    override protected val selectAll: Fragment = outer.selectAll

  def list(filters: Filter*): IO[List[Model]] = Repo.list(filters*).flatMap(_.traverse(toModel))

  def find(filters: Filter*): IO[Option[Model]] = Repo.find(filters*).flatMap(_.traverse(toModel))

  def get(filters: Filter*): IO[Model] = Repo.get(filters*).flatMap(toModel)