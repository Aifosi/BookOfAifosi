package bookofaifosi.commands

import bookofaifosi.Bot
import cats.effect.IO
import fs2.Stream

import scala.concurrent.duration.FiniteDuration

trait Streams:
  this: AnyCommand =>
  def stream: Stream[IO, Unit]

trait RepeatedStreams extends Streams:
  this: AnyCommand =>
  def repeatedStream(delay: FiniteDuration): Stream[IO, Unit]

  override lazy val stream: Stream[IO, Unit] = repeatedStream(Bot.config.checkFrequency)
