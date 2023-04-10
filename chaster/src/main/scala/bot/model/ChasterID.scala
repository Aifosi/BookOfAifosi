package bot.model

import io.circe.{Decoder, Encoder}
import org.http4s.Uri.Path.SegmentEncoder

opaque type ChasterID = String

object ChasterID:
  given Encoder[ChasterID] = Encoder.encodeString

  given Decoder[ChasterID] = Decoder.decodeString

  given SegmentEncoder[ChasterID] = SegmentEncoder.stringSegmentEncoder

  def apply(id: String): ChasterID = id
