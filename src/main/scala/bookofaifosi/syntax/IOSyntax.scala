package bookofaifosi.syntax

import bookofaifosi.Bot
import cats.effect.IO
import fs2.Stream
import bookofaifosi.syntax.logger.*

trait IOSyntax:
  extension [A](io: IO[A])
    def streamed: Stream[IO, A] = Stream.eval(io)
    def logError(default: => A): IO[A] = io.attempt.flatMap(_.fold(error => Bot.logger.error(error.getMessage).as(default), IO.pure))
