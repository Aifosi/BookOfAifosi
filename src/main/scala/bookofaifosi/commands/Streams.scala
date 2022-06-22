package bookofaifosi.commands

import cats.effect.IO
import fs2.Stream

import scala.concurrent.duration.FiniteDuration

trait Streams:
  this: AnyCommand =>
  def stream(delay: FiniteDuration): Stream[IO, Unit]
