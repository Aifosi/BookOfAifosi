package bot.commands

import bot.db.Filters.*
import bot.db.RegisteredUserRepository
import bot.model.{ChasterID, RegisteredUser}
import bot.model.event.ReactionEvent

import cats.data.EitherT
import cats.effect.IO
import scala.util.matching.Regex

object Voter:
  def getRegisteredUserAndChasterID(
    pattern: Regex,
    registeredUserRepository: RegisteredUserRepository,
    event: ReactionEvent,
  ): EitherT[IO, String, (RegisteredUser, ChasterID)] = for
    message   <- EitherT.liftF(event.message)
    chasterID <-
      pattern
        .findFirstMatchIn(message.content)
        .map(_.group(1))
        .fold(EitherT.leftT[IO, ChasterID](s"Could not extract id from $message")) { chasterId =>
          EitherT.pure[IO, String](ChasterID(chasterId))
        }
    author    <- EitherT.liftF(event.authorMember)
    user      <- registeredUserRepository
                   .find(author.equalDiscordAndGuildID)
                   .toRight(s"Could not find registered used ${event.author}")
  yield (user, chasterID)
