package bookofaifosi.syntax

import bookofaifosi.Bot
import cats.effect.IO
import fs2.Stream
import bookofaifosi.syntax.logger.*
import org.typelevel.log4cats.Logger

trait IOSyntax:
  extension [A](io: IO[A])
    def streamed: Stream[IO, A] = Stream.eval(io)
    def logError(default: => A)(using Logger[IO]): IO[A] = io.attempt.flatMap(_.fold(error => Logger[IO].error(error.getMessage).as(default), IO.pure))
