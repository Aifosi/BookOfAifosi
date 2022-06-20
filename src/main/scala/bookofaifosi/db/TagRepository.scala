package bookofaifosi.db

import bookofaifosi.Bot
import bookofaifosi.model.Tag
import cats.effect.IO
import doobie.{ConnectionIO, Fragment}

import java.util.UUID
import doobie.syntax.string.*
import doobie.postgres.implicits.*
import cats.syntax.functor.*
import doobie.util.log.LogHandler
import doobie.syntax.connectionio.*

object TagRepository extends Repository[Tag]:
  protected val selectAll = fr"select name, description from tags"
  def add(name: String, description: Option[String]): IO[Unit] =
    sql"insert into tags(name, description) values ($name, $description)".updateWithLogHandler(LogHandler.jdkLogHandler).run.void.transact(Bot.xa)

  def remove(name: String): IO[Int] =
    sql"delete from tags where name = $name".updateWithLogHandler(LogHandler.jdkLogHandler).run.transact(Bot.xa)

  def update(name: String, newName: Option[String], newDescription: Option[Option[String]]): IO[Int] =
    /*val fragment = List(updateName(newName), updateDescription(newDescription)).mkFragment(fr"update tags set", fr",", fr"where name = $name")
    if fragment == Fragment.empty then
      ConnectionIO.pure(0)
    else
      (fr"update tags set" ++ fragment ++ fr"where name = $name").update.run*/

    List(nameEqual(newName), descriptionEqual(newDescription)).mkFragment(fr"update tags set", fr",", fr"where name = $name").updateWithLogHandler(LogHandler.jdkLogHandler).run.transact(Bot.xa)

