package bookofaifosi.syntax

import bookofaifosi.Bot
import bookofaifosi.model.{Channel, Message}
import cats.effect.IO
import fs2.Stream
import bookofaifosi.syntax.logger.*
import bookofaifosi.syntax.stream.*
import cats.data.OptionT
import org.typelevel.log4cats.Logger

import scala.annotation.targetName

trait IOSyntax:
  extension [A](io: IO[IterableOnce[A]])
    def streamedIterable: Stream[IO, A] = Stream.evalI(io)

  extension [A](io: IO[A])
    def streamed: Stream[IO, A] = Stream.eval(io)
    def logError(default: => A)(using Logger[IO]): IO[A] = io.attempt.flatMap(_.fold(error => Logger[IO].error(error.getMessage).as(default), IO.pure))

  extension (maybeChannel: OptionT[IO, Channel])
    def sendMessage(string: String): OptionT[IO, Message] = maybeChannel.semiflatMap(_.sendMessage(string))
