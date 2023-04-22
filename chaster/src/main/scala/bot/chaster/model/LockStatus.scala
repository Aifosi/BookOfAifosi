package bot.chaster.model

import io.circe.Decoder
import scala.util.Try

enum LockStatus:
  case Locked, Unlocked, Deserted

object LockStatus:
  given Decoder[LockStatus] = Decoder[String].emapTry(string => Try(LockStatus.valueOf(string.capitalize)))
