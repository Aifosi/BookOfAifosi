package bookofaifosi.syntax

import bookofaifosi.Bot
import cats.effect.IO
import cats.{Applicative, Functor, Show}
import cats.syntax.functor.*
import cats.syntax.show.*
import cats.syntax.traverse.*
import bookofaifosi.syntax.logger.*
import fs2.Stream

trait StreamSyntax:
  extension (stream: Stream.type)
    def evalOption[F[_]: Functor, A](a: => F[Option[A]]): Stream[F, A] = Stream.evalSeq(a.map(_.toSeq))
    def filter[F[_]](cond: Boolean): Stream[F, Unit] = if cond then Stream.emit(()) else Stream.empty
    def when[F[_], A](cond: Boolean)(a: => A): Stream[F, A] = Stream.emits(Option.when(cond)(a).toList)
    def whenS[F[_], A](cond: Boolean)(a: => IterableOnce[A]): Stream[F, A] = Stream.emits(Option.when(cond)(a).toList.flatten)
    def whenF[F[_]: Functor, A](cond: Boolean)(a: => F[A]): Stream[F, A] = evalOption(a.map(Option.when(cond)))
    def emit[F[_], A](a: => Iterable[A]): Stream[F, A] = Stream.emits(a.toSeq)
    def println[A](a: A)(using S: Show[A] = Show.fromToString[A]): Stream[IO, Unit] = Stream.eval(IO.println(a))
    def unit: Stream[IO, Unit] = Stream.eval(IO.unit)