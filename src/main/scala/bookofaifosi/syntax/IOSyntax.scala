package bookofaifosi.syntax

import cats.effect.IO
import fs2.Stream

trait IOSyntax:
  extension [A](io: IO[A])
    def streamed: Stream[IO, A] = Stream.eval(io)
