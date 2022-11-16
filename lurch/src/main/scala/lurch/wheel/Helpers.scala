package lurch.wheel

import bot.chaster.Lock
import bot.db.RegisteredUserRepository
import bot.model.{ChasterID, RegisteredUser}
import cats.data.OptionT
import cats.effect.IO
import org.typelevel.log4cats.Logger

import bot.chaster.Client.{*, given}
import bot.db.Filters.*
import cats.syntax.traverse.*

def lockAndKeyholder(user: RegisteredUser, lockID: ChasterID)(using Logger[IO]): OptionT[IO, (Lock, RegisteredUser)] =
  for
    lock <- OptionT.liftF(user.lock(lockID))
    keyholder <- OptionT(lock.keyholder.flatTraverse(keyholder => RegisteredUserRepository.find(keyholder._id.equalChasterID)))
  yield (lock, keyholder)
