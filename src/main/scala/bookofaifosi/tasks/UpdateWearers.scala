package bookofaifosi.tasks

import bookofaifosi.chaster.LockStatus
import bookofaifosi.db.RegisteredUserRepository
import bookofaifosi.db.Filters.*
import bookofaifosi.syntax.all.*
import bookofaifosi.chaster.Client.*
import bookofaifosi.chaster.Client.given
import bookofaifosi.model.RegisteredUser
import cats.effect.IO
import fs2.Stream
import org.typelevel.log4cats.Logger

import scala.concurrent.duration.FiniteDuration

object UpdateWearers extends RepeatedStreams:
  private def updateUser(user: RegisteredUser, keyholderIDs: List[String], isLocked: Boolean): IO[Unit] =
    if user.keyholderIDs != keyholderIDs || user.isLocked != isLocked then
      RegisteredUserRepository.update(user.id, keyholderIDs, isLocked, user.isWearer, user.isKeyholder, user.token.id).void
    else
      IO.unit

  override def repeatedStream(delay: FiniteDuration)(using Logger[IO]): Stream[IO, Unit] =
    for
      _ <- Stream.awakeEvery[IO](delay)
      user <- Stream.evalSeq(RegisteredUserRepository.list(isWearer))
      profile <- user.publicProfileByName(user.chasterName).streamed
      _ <- Stream.filter(profile.exists(!_.isDisabled))
      locks <- user.locks.streamed
      keyholderIDs = locks.flatMap(_.keyholder.map(_._id))
      isLocked = locks.exists(_.status == LockStatus.Locked)
      _ <- updateUser(user, keyholderIDs, isLocked).streamed
    yield ()
