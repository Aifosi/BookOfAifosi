package bookofaifosi.tasks

import bookofaifosi.db.{ChasterName, ChasterNamesRepository, RegisteredUserRepository}
import bookofaifosi.db.Filters.*
import cats.effect.IO
import fs2.Stream
import org.typelevel.log4cats.Logger
import bookofaifosi.syntax.all.*
import bookofaifosi.chaster.Client.*
import bookofaifosi.chaster.Client.given

object GetChasterID extends Streams:
  override def stream(using Logger[IO]): Stream[IO, Unit] =
    for
      ChasterName(id, chasterName) <- Stream.evalSeq(ChasterNamesRepository.list())
      user <- RegisteredUserRepository.get(id.equalID).streamed
      maybeProfile <- user.publicProfileByName(chasterName).streamed
      _ <- maybeProfile.fold(RegisteredUserRepository.remove(id.equalID))(profile => RegisteredUserRepository.updateChaster(id, profile._id)).streamed
    yield ()

