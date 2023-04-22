package bot.chaster.model

import io.circe.Decoder
import org.http4s.Status
import scala.util.control.NoStackTrace

case class ChasterAPIError(
  statusCode: Status,
  message: String,
  error: String,
) extends Throwable(s"Chaster API error $statusCode - $error: $message") with NoStackTrace
    derives Decoder

given Decoder[Status] = Decoder[Int].emap(status => Status.fromInt(status).left.map(_.message))
