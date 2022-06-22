package bookofaifosi

package object model:
  opaque type DiscordID = Long
  
  object DiscordID:
    def apply(id: Long): DiscordID = id
    
  extension (id: DiscordID)
    def toLong: Long = id
    
  given Conversion[Long, DiscordID] = DiscordID.apply
