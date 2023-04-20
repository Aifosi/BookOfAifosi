package bot.syntax

import bot.chaster.ChasterClient
import bot.model.{RegisteredUser, UserToken}
import cats.{FlatMap, ~>}
import cats.syntax.flatMap.*
import cats.data.Kleisli
import cats.effect.IO
import fs2.Stream
import bot.instances.functionk.toStream

trait KleisliSyntax:
  extension[F[_] : FlatMap, B] (kleisli: Kleisli[F, UserToken, B])
    def runUsingTokenOf(user: RegisteredUser)(using f: IO ~> F): F[B] =
      f(user.updatedToken).flatMap(kleisli.run)
  extension[A, B] (kleisli: Kleisli[IO, A, B])
    def toKleisliStream: Kleisli[[O] =>> Stream[IO, O], A, B] = kleisli.mapK(toStream)
