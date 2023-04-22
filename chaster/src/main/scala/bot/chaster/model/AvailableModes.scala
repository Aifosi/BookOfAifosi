package bot.chaster.model

import bot.chaster.model.instances.*

import io.circe.{Decoder, Encoder}
import scala.util.Try

enum AvailableModes:
  case Unlimited, Cumulative, NonCumulative, Turn

object AvailableModes:
  given Decoder[AvailableModes] = Decoder[String].emapTry { string =>
    Try(AvailableModes.valueOf(string.snakeToPascalCase))
  }
  given Encoder[AvailableModes] = Encoder[String].contramap(_.toString.pascalToSnakeCase)
