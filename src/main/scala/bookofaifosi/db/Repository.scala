package bookofaifosi.db

import bookofaifosi.Bot
import cats.effect.IO
import doobie.{ConnectionIO, Fragment}
import doobie.implicits.*
import doobie.postgres.*
import doobie.postgres.implicits.*
import doobie.util.Read
import doobie.util.log.LogHandler

import scala.util.chaining.*

trait Filters[A: Read]:
  protected def nameSimilar(name: Option[String]): Option[Fragment] = name.map(name => fr"name ILIKE $name")

  protected def partialNameSimilar(name: Option[String]): Option[Fragment] = name.map(name => fr"name ILIKE ${s"%$name%"}")

  protected def nameEqual(name: Option[String]): Option[Fragment] = name.map(name => fr"name = $name")

  protected def descriptionEqual(description: Option[Option[String]]): Option[Fragment] = description.map(description => fr"description = ${description.orNull}")

trait Repository[A: Read] extends Filters[A]:
  protected val selectAll: Fragment

  private def query(name: Option[String], partialName: Option[String] = None) =
    (selectAll ++ List(nameSimilar(name), partialNameSimilar(partialName)).mkFragment(fr"where", fr"and")).queryWithLogHandler[A](LogHandler.jdkLogHandler)

  def list(name: Option[String] = None, partialName: Option[String] = None): IO[List[A]] =
    query(
      name = name,
      partialName = partialName,
    ).to[List].transact(Bot.xa)

  def find(name: String): IO[Option[A]] =
    query(name = Some(name)).option.transact(Bot.xa)

extension (fragments: List[Option[Fragment]])
  def mkFragment(start: Fragment = Fragment.empty, sep: Fragment, end: Fragment = Fragment.empty): Fragment =
    val flattened = fragments.flatten
    if flattened.isEmpty then
      Fragment.empty
    else
      start ++ flattened.reduceLeft(_ ++ sep ++ _) ++ end