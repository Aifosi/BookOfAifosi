package bookofaifosi.syntax

import bookofaifosi.Bot
import cats.effect.IO
import cats.{Functor, Show}
import cats.syntax.functor.*
import cats.syntax.show.*
import bookofaifosi.syntax.logger.*
import fs2.Stream

trait StreamSyntax:
  extension (stream: Stream.type)
    def when[F[_], A](cond: Boolean)(a: => A): Stream[F, A] = Stream.emits(Option.when(cond)(a).toList)
    def whenS[F[_], A](cond: Boolean)(a: => IterableOnce[A]): Stream[F, A] = Stream.emits(Option.when(cond)(a).toList.flatten)
    def emit[F[_], A](a: => Iterable[A]): Stream[F, A] = Stream.emits(a.toSeq)
    def evalOption[F[_]: Functor, A](a: => F[Option[A]]): Stream[F, A] = Stream.evalSeq(a.map(_.toSeq))
    def println[A](a: A)(using S: Show[A] = Show.fromToString[A]): Stream[IO, Unit] = Stream.eval(IO.println(a))