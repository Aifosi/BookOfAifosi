package bookofaifosi.db

import doobie.{ConnectionIO, Fragment}
import doobie.implicits.*
import doobie.postgres.*
import doobie.postgres.implicits.*
import doobie.util.Read

trait Filters[A: Read]:
  protected val selectAll: Fragment

  protected lazy val filter = selectAll ++ fr"where true"

  protected def filterName(name: Option[String]) = name.fold(Fragment.empty)(name => fr"and name ILIKE $name")

  protected def filterPartialName(name: Option[String]) = name.fold(Fragment.empty)(name => fr"and name ILIKE ${s"%$name%"}")

trait Listing[A: Read] extends Filters[A]:
  private def query(name: Option[String], partialName: Option[String] = None) =
    (filter ++ filterName(name) ++ filterPartialName(partialName)).query[A]

  def list(name: Option[String] = None, partialName: Option[String] = None): ConnectionIO[List[A]] =
    query(
      name = name,
      partialName = partialName,
    ).to[List]

  def find(name: String): ConnectionIO[Option[A]] =
    query(name = Some(name)).option