package bookofaifosi

import pureconfig.ConfigReader

package object model:
  opaque type DiscordID = Long
  
  object DiscordID:
    given ConfigReader[DiscordID] = ConfigReader.longConfigReader
    def apply(id: Long): DiscordID = id

  extension (id: DiscordID)
    def toLong: Long = id

  given Conversion[Long, DiscordID] = DiscordID.apply
