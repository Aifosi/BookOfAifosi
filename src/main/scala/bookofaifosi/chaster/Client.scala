package bookofaifosi.chaster

import bookofaifosi.Bot
import cats.effect.IO
import org.http4s.*
import org.http4s.dsl.io.*
import org.http4s.circe.CirceEntityDecoder.*
import org.http4s.Method.*
import org.http4s.client.dsl.io.*
import org.http4s.client.Client as HTTPClient
import org.http4s.headers.Authorization

import java.time.OffsetDateTime

case class Client(client: HTTPClient[IO], token: String, expiresAt: OffsetDateTime, refreshToken: String):
  private val baseUri = Uri.unsafeFromString("https://api.chaster.app")
  val profile: IO[Profile] = client.expect[Profile](GET(baseUri / "auth" / "profile", Authorization(Credentials.Token(AuthScheme.Bearer, token))))
