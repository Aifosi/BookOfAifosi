package bot.commands

import bot.chaster.ChasterClient
import bot.chaster.model.VoteAction
import bot.db.Filters.*
import bot.db.RegisteredUserRepository
import bot.model.event.ReactionEvent

import cats.effect.IO
import org.typelevel.log4cats.Logger

class VoterAdd(
  chasterClient: ChasterClient,
  registeredUserRepository: RegisteredUserRepository,
) extends ReactionCommand with Hidden with NoLog:
  override def pattern: String = PilloryVoter.add

  override def apply(pattern: String, event: ReactionEvent)(using Logger[IO]): IO[Boolean] =
    SessionVoter
      .vote(chasterClient, registeredUserRepository, event, VoteAction.Add)
      .orElse(PilloryVoter.vote(chasterClient, registeredUserRepository, event))
      .leftSemiflatTap(error => Logger[IO].debug(error))
      .fold(
        _ => false,
        _ => true,
      )
