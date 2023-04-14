package bot.model

import doobie.util.{Read, Write}
import doobie.{Get, Put}
import pureconfig.ConfigReader

opaque type DiscordID = Long

object DiscordID:
  given Read[DiscordID] = Read.fromGet[Long]

  given Write[DiscordID] = Write.fromPut[Long]
  
  given Get[DiscordID] = Instances.getLong.map(DiscordID(_))

  given Put[DiscordID] = Instances.putLong.contramap(_.toLong)
  given ConfigReader[DiscordID] = ConfigReader.longConfigReader

  def apply(id: Long): DiscordID = id

extension (id: DiscordID)
  def toLong: Long = id

given Conversion[Long, DiscordID] = DiscordID.apply
