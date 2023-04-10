package bot.model

import doobie.util.{Read, Write}

object Instances:
  import doobie.postgres.implicits.*
  lazy val stringListRead: Read[List[String]] = Read[List[String]]
  lazy val stringListWrite: Write[List[String]] = Write[List[String]]
  lazy val stringOptionRead: Read[Option[String]] = Read[Option[String]]
  lazy val stringOptionWrite: Write[Option[String]] = Write[Option[String]]
