package bookofaifosi.syntax

import cats.Show
import cats.syntax.show.*
import cats.effect.{Deferred, IO}
import org.typelevel.log4cats.Logger

trait LoggerSyntax:
  extension (deferedLogger: Deferred[IO, Logger[IO]])
    def error[A](a: A)(using S: Show[A] = Show.fromToString[A]): IO[Unit] = deferedLogger.get.flatMap(_.error(a.show))
    def warn[A](a: A)(using S: Show[A] = Show.fromToString[A]): IO[Unit] = deferedLogger.get.flatMap(_.warn(a.show))
    def info[A](a: A)(using S: Show[A] = Show.fromToString[A]): IO[Unit] = deferedLogger.get.flatMap(_.info(a.show))
    def debug[A](a: A)(using S: Show[A] = Show.fromToString[A]): IO[Unit] = deferedLogger.get.flatMap(_.debug(a.show))
    def trace[A](a: A)(using S: Show[A] = Show.fromToString[A]): IO[Unit] = deferedLogger.get.flatMap(_.trace(a.show))
