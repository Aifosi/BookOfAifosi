package bot.model

import doobie.{Get, Put, Read, Write}

object Instances:
  import doobie.postgres.implicits.*
  lazy val getLong: Get[Long]                       = Get[Long]
  lazy val putLong: Put[Long]                       = Put[Long]
  lazy val stringListRead: Read[List[String]]       = Read[List[String]]
  lazy val stringListWrite: Write[List[String]]     = Write[List[String]]
  lazy val stringOptionRead: Read[Option[String]]   = Read[Option[String]]
  lazy val stringOptionWrite: Write[Option[String]] = Write[Option[String]]
