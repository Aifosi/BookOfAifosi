package bot.chaster.model

import io.circe.{Decoder, Encoder}
import scala.util.Try

enum VoteAction:
  case Add, Remove, Random

object VoteAction:
  given Decoder[VoteAction] = Decoder[String].emapTry { string =>
    Try(VoteAction.valueOf(string.capitalize))
  }
  given Encoder[VoteAction] = Encoder[String].contramap(_.toString.toLowerCase)
