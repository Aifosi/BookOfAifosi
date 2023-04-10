package bot.db

import bot.utils.Maybe
import cats.data.{EitherT, OptionT}
import cats.effect.IO
import cats.syntax.option.*
import cats.syntax.traverse.*
import doobie.*
import doobie.implicits.*
import doobie.postgres.*
import doobie.postgres.implicits.*
import doobie.util.Read
import fs2.Stream

trait ThoroughList[DB: Read, Model, ID]:
  this: ModelRepository[DB, Model] =>
  def id(db: DB): ID
  def thoroughList(filters: Filter*): IO[List[Either[ID, Model]]] =
    Repo.list(filters *).transact(this.transactor).flatMap(_.traverse(dbModel => toModel(dbModel).leftMap(_ => id(dbModel)).value))

trait ModelRepository[DB: Read, Model](using protected val transactor: Transactor[IO], logHandler: LogHandler) extends RepositoryFields:
  outer =>
  def toModel(a: DB): Maybe[Model]

  final def unsafeToModel(a: DB): IO[Model] = toModel(a).rethrowT

  private[db] object Repo extends Repository[DB]:
    override protected val table: Fragment = outer.table
    override protected val columns: List[String] = outer.columns

  inline private def toModelList(dbModel: DB): IO[List[Model]] = toModel(dbModel).value.map(_.toSeq.toList)

  def list(filters: Filter*): IO[List[Model]] =
    Repo.list(filters *).transact(transactor).flatMap(_.flatTraverse(toModelList(_)))

  def find(filters: Filter*): OptionT[IO, Model] =
    Repo.find(filters *).transact(transactor).flatMap(toModel(_).toOption)

  def get(filters: Filter*): IO[Model] =
    Repo.get(filters *).transact(transactor).flatMap(unsafeToModel)

  def update(updates: Filter*)(where: Fragment, more: Fragment*): IO[Model] =
    Repo.update(updates *)(where, more *).transact(transactor).flatMap(unsafeToModel)

  def updateMany(updates: Filter*)(where: Fragment, more: Fragment*): IO[List[Model]] =
    Repo.updateMany(updates *)(where, more *).transact(transactor).flatMap(_.traverse(unsafeToModel))

  def remove(filter: Filter, moreFilters: Filter*): IO[Int] =
    Repo.remove(filter, moreFilters*).transact(transactor)

  def insertOne[Info: Write](info: Info)(columns: String*): IO[DB] =
    Repo.insertOne(info)(columns*).transact(transactor)

  /*def insertMany[Info: Write](info: List[Info])(columns: String*): Stream[IO, DB] =
    Repo.insertMany(info)(columns*).transact(transactor)*/
