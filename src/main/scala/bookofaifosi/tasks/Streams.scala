package bookofaifosi.tasks

import bookofaifosi.Bot
import cats.effect.IO
import fs2.Stream
import org.typelevel.log4cats.Logger

import scala.concurrent.duration.FiniteDuration

trait Streams:
  def stream(using Logger[IO]): Stream[IO, Unit]

trait RepeatedStreams extends Streams:
  def repeatedStream(delay: FiniteDuration)(using Logger[IO]): Stream[IO, Unit]

  override def stream(using Logger[IO]): Stream[IO, Unit] = repeatedStream(Bot.config.checkFrequency)
