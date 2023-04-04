package bot

import com.comcast.ip4s.{Host, Port}
import com.typesafe.config.{Config, ConfigFactory}
import pureconfig.generic.derivation.default.derived
import pureconfig.{ConfigReader, ConfigSource}
import pureconfig.error.CannotConvert

case class ChasterConfiguration(
  host: Host,
  port: Port,
  publicHost: String,
  publicPort: Int,
  clientId: String,
  secretKey: String,
)derives ConfigReader

object ChasterConfiguration:
  given ConfigReader[Host] = ConfigReader[String].emap { host =>
    Host.fromString(host).toRight(CannotConvert(host, "Host", "Is not a valid host."))
  }
  given ConfigReader[Port] = ConfigReader[Int].emap { port =>
    Port.fromInt(port).toRight(CannotConvert(port.toString, "Port", "Is not a valid port number, either too large or too small."))
  }

  def fromConfig(config: Config = ConfigFactory.load()): ChasterConfiguration =
    ConfigSource.fromConfig(config).at("chaster").loadOrThrow[ChasterConfiguration]
