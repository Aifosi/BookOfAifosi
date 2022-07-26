package bookofaifosi

import doobie.util.{Read, Write, Get, Put}
import io.circe.{Decoder, Encoder}
import org.http4s.Uri.Path.SegmentEncoder
import pureconfig.ConfigReader

object Instances:
  import doobie.postgres.implicits.*
  lazy val stringListRead: Read[List[String]] = Read[List[String]]
  lazy val stringListWrite: Write[List[String]] = Write[List[String]]

package object model:
  opaque type DiscordID = Long
  
  object DiscordID:
    given ConfigReader[DiscordID] = ConfigReader.longConfigReader
    def apply(id: Long): DiscordID = id

  extension (id: DiscordID)
    def toLong: Long = id

  given Conversion[Long, DiscordID] = DiscordID.apply

  opaque type ChasterID = String

  object ChasterID:
    given Encoder[ChasterID] = Encoder.encodeString
    given Decoder[ChasterID] = Decoder.decodeString
    given SegmentEncoder[ChasterID] = SegmentEncoder.stringSegmentEncoder
    given Read[ChasterID] = Read.fromGet[String]
    given Write[ChasterID] = Write.fromPut[String]
    given Read[List[ChasterID]] = Instances.stringListRead
    given Write[List[ChasterID]] = Instances.stringListWrite
    def apply(id: String): ChasterID = id