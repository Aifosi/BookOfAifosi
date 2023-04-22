package bot.model

import doobie.{Get, Put, Read, Write}
import io.circe.{Decoder, Encoder}
import org.http4s.Uri.Path.SegmentEncoder
import pureconfig.ConfigReader

given Read[ChasterID]          = Read.fromGet[String].asInstanceOf
given Write[ChasterID]         = Write.fromPut[String].asInstanceOf
given Read[List[ChasterID]]    = Instances.stringListRead.asInstanceOf
given Write[List[ChasterID]]   = Instances.stringListWrite.asInstanceOf
given Read[Option[ChasterID]]  = Instances.stringOptionRead.asInstanceOf
given Write[Option[ChasterID]] = Instances.stringOptionWrite.asInstanceOf
