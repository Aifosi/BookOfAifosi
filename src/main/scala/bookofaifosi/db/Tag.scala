package bookofaifosi.db

import bookofaifosi.Bot
import cats.effect.IO

import java.util.UUID
import doobie.implicits.*
import doobie.postgres.*
import doobie.postgres.implicits.*

case class Tag(
  id: UUID,
  name: String,
  description: Option[String],
)

object Tag extends Listing[Tag]:
  protected val selectAll = fr"select id, name, description from tags"
  def add(name: String, description: Option[String]): IO[Unit] =
    sql"insert into tags(name, description) values ($name, $description)".update.run.transact(Bot.xa).void

