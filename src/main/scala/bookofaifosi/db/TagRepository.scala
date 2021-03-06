package bookofaifosi.db

import bookofaifosi.Bot
import bookofaifosi.model.Tag
import cats.effect.IO
import doobie.{ConnectionIO, Fragment}

import java.util.UUID
import doobie.syntax.string.*
import doobie.postgres.implicits.*
import cats.syntax.functor.*
import cats.syntax.option.*
import doobie.util.log.LogHandler
import doobie.syntax.connectionio.*
import bookofaifosi.db.Filters.*

object TagRepository extends Repository[Tag]:
  override protected val table: Fragment = fr"tags"
  override protected val selectColumns: Fragment = fr"name, description"
  def add(name: String, description: Option[String]): IO[Unit] =
    sql"insert into $table (name, description) values ($name, $description)"
      .update
      .run
      .void
      .transact(Bot.xa)

  def update(name: String, newName: Option[String], newDescription: Option[Option[String]]): IO[Int] =
    List(newName.equalName, descriptionEqual(newDescription)).mkFragment(fr"update tags set", fr",", fr"where name = $name")
      .update
      .run
      .transact(Bot.xa)
