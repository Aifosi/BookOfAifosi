package bot.utils

import cats.data.EitherT
import cats.effect.IO

type Maybe[T] = EitherT[IO, Exception, T]
