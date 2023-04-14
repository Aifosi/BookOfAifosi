package bot.commands

import bot.chaster.{ChasterClient, VoteAction}
import bot.db.RegisteredUserRepository
import bot.db.Filters.*
import bot.model.event.ReactionEvent
import cats.effect.IO
import org.typelevel.log4cats.Logger

class SessionVoteRemove(
  chasterClient: ChasterClient,
  registeredUserRepository: RegisteredUserRepository,
) extends ReactionCommand with Hidden with NoLog:
  override def pattern: String = SessionVoter.remove

  override def apply(pattern: String, event: ReactionEvent)(using Logger[IO]): IO[Boolean] =
    SessionVoter.vote(chasterClient, registeredUserRepository, event, VoteAction.Remove)
      .foldF(
        error => Logger[IO].debug(error).as(false),
        _ => IO.pure(true),
      )
