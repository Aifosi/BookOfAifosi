package bot.commands

import bot.chaster.ChasterClient
import bot.chaster.model.VoteAction
import bot.db.Filters.*
import bot.db.RegisteredUserRepository
import bot.model.event.ReactionEvent

import cats.effect.IO
import org.typelevel.log4cats.Logger

class SessionVoteRemove(
  chasterClient: ChasterClient,
  registeredUserRepository: RegisteredUserRepository,
) extends ReactionCommand with Hidden:
  override def pattern: String = SessionVoter.remove

  override def apply(pattern: String, event: ReactionEvent)(using Logger[IO]): IO[Boolean] =
    SessionVoter
      .vote(chasterClient, registeredUserRepository, event, VoteAction.Remove)
      .foldF(
        error => Logger[IO].debug(error).as(false),
        _ => IO.pure(true),
      )
