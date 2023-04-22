package bot.chaster

import cats.effect.IO
import com.comcast.ip4s.{Host, Port}
import com.typesafe.config.{Config, ConfigFactory}
import org.http4s.Uri
import pureconfig.{ConfigReader, ConfigSource}
import pureconfig.error.CannotConvert
import pureconfig.generic.derivation.default.derived
import pureconfig.module.catseffect.syntax.*

case class ChasterConfiguration(
  host: Host,
  port: Port,
  publicHost: String,
  publicPort: Int,
  clientId: String,
  secretKey: String,
  authUri: Uri,
  apiUri: Uri,
) derives ConfigReader

object ChasterConfiguration:
  given ConfigReader[Host] = ConfigReader[String].emap { host =>
    Host.fromString(host).toRight(CannotConvert(host, "Host", "Is not a valid host."))
  }
  given ConfigReader[Port] = ConfigReader[Int].emap { port =>
    Port
      .fromInt(port)
      .toRight(CannotConvert(port.toString, "Port", "Is not a valid port number, either too large or too small."))
  }
  given ConfigReader[Uri]  = ConfigReader[String].emap { uri =>
    Uri.fromString(uri).left.map { parsingFailure =>
      CannotConvert(uri, "Uri", parsingFailure.message)
    }
  }

  def fromConfig(config: Config = ConfigFactory.load()): IO[ChasterConfiguration] =
    ConfigSource.fromConfig(config).at("chaster").loadF[IO, ChasterConfiguration]()
