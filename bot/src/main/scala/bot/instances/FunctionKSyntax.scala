package bot.instances

import cats.arrow.FunctionK
import cats.data.{EitherT, OptionT}
import cats.effect.IO
import fs2.Stream

trait FunctionKSyntax:
  given toStream: FunctionK[IO, [A] =>> Stream[IO, A]] = new FunctionK[IO, [A] =>> Stream[IO, A]]:
    override def apply[A](fa: IO[A]): Stream[IO, A] = Stream.eval(fa)

  given [E]: FunctionK[IO, [A] =>> EitherT[IO, E, A]] = new FunctionK[IO, [A] =>> EitherT[IO, E, A]]:
    override def apply[A](fa: IO[A]): EitherT[IO, E, A] = EitherT.liftF(fa)

  given FunctionK[IO, [A] =>> OptionT[IO, A]] = new FunctionK[IO, [A] =>> OptionT[IO, A]]:
    override def apply[A](fa: IO[A]): OptionT[IO, A] = OptionT.liftF(fa)

  given FunctionK[IO, IO] = new FunctionK[IO, IO]:
    override def apply[A](fa: IO[A]): IO[A] = fa
