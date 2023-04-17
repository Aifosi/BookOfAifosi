package bot.commands

import bot.chaster.{ChasterClient, VoteAction}
import bot.db.RegisteredUserRepository
import bot.db.Filters.*
import bot.model.event.ReactionEvent
import cats.effect.IO
import org.typelevel.log4cats.Logger

class SessionVoteRandom(
  chasterClient: ChasterClient,
  registeredUserRepository: RegisteredUserRepository,
) extends ReactionCommand with Hidden:
  override def pattern: String = SessionVoter.random

  override def apply(pattern: String, event: ReactionEvent)(using Logger[IO]): IO[Boolean] =
    SessionVoter.vote(chasterClient, registeredUserRepository, event, VoteAction.Random)
      .foldF(
        error => Logger[IO].debug(error).as(false),
        _ => IO.pure(true),
      )
