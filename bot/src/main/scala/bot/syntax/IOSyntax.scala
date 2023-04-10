package bot.syntax

import bot.Bot
import bot.model.{Channel, Message}
import cats.effect.IO
import fs2.Stream
import bot.syntax.stream.*
import cats.data.OptionT
import cats.syntax.applicative.*
import cats.syntax.option.*
import org.typelevel.log4cats.Logger

import scala.annotation.targetName

trait IOSyntax:
  extension [A](io: IO[IterableOnce[A]])
    def streamedIterable: Stream[IO, A] = Stream.evalI(io)

  extension [A](io: IO[A])
    def streamed: Stream[IO, A] = Stream.eval(io)
    def logError(default: => A)(using Logger[IO]): IO[A] = io.attempt.flatMap(_.fold(error => Logger[IO].error(error)(error.getMessage).as(default), _.pure))
    def logErrorOption(using Logger[IO]): IO[Option[A]] = io.attempt.flatMap(_.fold(error => Logger[IO].error(error)(error.getMessage).as(None), _.some.pure))
