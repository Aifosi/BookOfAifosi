package bookofaifosi.db

import bookofaifosi.Bot
import cats.effect.IO
import doobie.{ConnectionIO, Fragment}

import java.util.UUID
import doobie.syntax.string.*
import doobie.postgres.implicits.*
import cats.syntax.functor.*
import doobie.util.log.LogHandler

case class Tag(
  id: UUID,
  name: String,
  description: Option[String],
)

object Tag extends Queries[Tag]:
  protected val selectAll = fr"select id, name, description from tags"
  def add(name: String, description: Option[String]): ConnectionIO[Unit] =
    sql"insert into tags(name, description) values ($name, $description)".update.run.void

  def remove(name: String): ConnectionIO[Int] =
    sql"delete from tags where name = $name".update.run

  def update(name: String, newName: Option[String], newDescription: Option[Option[String]]): ConnectionIO[Int] =
    /*val fragment = List(updateName(newName), updateDescription(newDescription)).mkFragment(fr"update tags set", fr",", fr"where name = $name")
    if fragment == Fragment.empty then
      ConnectionIO.pure(0)
    else
      (fr"update tags set" ++ fragment ++ fr"where name = $name").update.run*/

    List(updateName(newName), updateDescription(newDescription)).mkFragment(fr"update tags set", fr",", fr"where name = $name").updateWithLogHandler(LogHandler.jdkLogHandler).run

