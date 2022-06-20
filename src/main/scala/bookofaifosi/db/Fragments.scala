package bookofaifosi.db

import doobie.{ConnectionIO, Fragment}
import doobie.implicits.*
import doobie.postgres.*
import doobie.postgres.implicits.*
import doobie.util.Read
import doobie.util.log.LogHandler
import scala.util.chaining.*

trait Fragments[A: Read]:
  protected val selectAll: Fragment

  protected def filterName(name: Option[String]): Option[Fragment] = name.map(name => fr"name ILIKE $name")

  protected def filterPartialName(name: Option[String]): Option[Fragment] = name.map(name => fr"name ILIKE ${s"%$name%"}")

  protected def updateName(name: Option[String]): Option[Fragment] = name.map(name => fr"name = $name")

  protected def updateDescription(description: Option[Option[String]]): Option[Fragment] = description.map(description => fr"description = ${description.orNull}")

trait Queries[A: Read] extends Fragments[A]:
  private def query(name: Option[String], partialName: Option[String] = None) =
    (selectAll ++ List(filterName(name), filterPartialName(partialName)).mkFragment(fr"where", fr"and")).queryWithLogHandler[A](LogHandler.jdkLogHandler)


  def list(name: Option[String] = None, partialName: Option[String] = None): ConnectionIO[List[A]] =
    query(
      name = name,
      partialName = partialName,
    ).to[List]

  def find(name: String): ConnectionIO[Option[A]] =
    query(name = Some(name)).option

extension (fragments: List[Option[Fragment]])
  def mkFragment(start: Fragment = Fragment.empty, sep: Fragment, end: Fragment = Fragment.empty): Fragment =
    val flattened = fragments.flatten
    if flattened.isEmpty then
      Fragment.empty
    else
      start ++ flattened.reduceLeft(_ ++ sep ++ _) ++ end