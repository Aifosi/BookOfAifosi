package bookofaifosi.commands

import cats.effect.IO
import fs2.Stream

trait Streams:
  this: AnyCommand =>
  def stream: Stream[IO, Unit]
