package bot.tasks

import bot.Bot
import cats.effect.IO
import fs2.Stream
import org.typelevel.log4cats.Logger

import scala.concurrent.duration.FiniteDuration

trait Streams:
  def stream: Stream[IO, Unit]

trait RepeatedStreams extends Streams:
  def delay: FiniteDuration
  def repeatedStream: Stream[IO, Unit]

  override def stream: Stream[IO, Unit] = Stream.awakeEvery[IO](delay) >> repeatedStream
