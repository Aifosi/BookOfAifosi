package bookofaifosi.db

import doobie.Fragment
import doobie.postgres.implicits.*
import doobie.syntax.string.*
import doobie.syntax.connectionio.*

import java.util.UUID

case class ChasterName(
  id: UUID,
  chasterName: String,
)

object ChasterNamesRepository extends Repository[ChasterName] {
  override protected val columns: List[String] = List("id", "chaster_name")
  override protected val table: Fragment = fr"user_chaster_names"
}
