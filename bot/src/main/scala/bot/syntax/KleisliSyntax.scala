package bot.syntax

import bot.chaster.ChasterClient
import bot.instances.functionk.toStream
import bot.model.{RegisteredUser, UserToken}

import cats.{~>, FlatMap}
import cats.data.Kleisli
import cats.effect.IO
import cats.syntax.flatMap.*
import fs2.Stream

trait KleisliSyntax:
  extension [F[_]: FlatMap, B](kleisli: Kleisli[F, UserToken, B])
    def runUsingTokenOf(user: RegisteredUser)(using f: IO ~> F): F[B] =
      f(user.updatedToken).flatMap(kleisli.run)
  extension [A, B](kleisli: Kleisli[IO, A, B])
    def toKleisliStream: Kleisli[[O] =>> Stream[IO, O], A, B]         = kleisli.mapK(toStream)
