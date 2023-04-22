package bot.commands

import bot.chaster.ChasterClient
import bot.chaster.model.{SharedLink, VoteAction}
import bot.commands.{Hidden, NoLog, TextCommand}
import bot.db.{RegisteredUserRepository, given}
import bot.instances.functionk.given
import bot.model.{ChasterID, *}
import bot.model.event.{MessageEvent, ReactionEvent}
import bot.syntax.io.*
import bot.syntax.kleisli.*
import bot.syntax.stream.*
import bot.tasks.Streams

import cats.data.{EitherT, Kleisli, OptionT}
import cats.effect.IO
import cats.syntax.foldable.*
import cats.syntax.option.*
import cats.syntax.traverse.*
import doobie.postgres.implicits.*
import doobie.syntax.string.*
import fs2.Stream
import java.time.{Instant, ZoneOffset}
import java.time.temporal.ChronoUnit
import org.typelevel.log4cats.Logger
import scala.concurrent.duration.*
import scala.util.chaining.*
import scala.util.matching.Regex

object SessionVoter extends TextCommand with Hidden with NoLog:
  val add    = "⏫"
  val remove = "⏬"
  val random = "\uD83D\uDD00"

  override val pattern: Regex = ".*http(?:s)?://chaster.app/sessions/(\\w{16}).*".r

  override def apply(pattern: Regex, event: MessageEvent)(using Logger[IO]): IO[Boolean] =
    for
      _ <- event.message.addReaction(add)
      _ <- event.message.addReaction(remove)
      _ <- event.message.addReaction(random)
    yield true

  def vote(
    client: ChasterClient,
    registeredUserRepository: RegisteredUserRepository,
    event: ReactionEvent,
    action: VoteAction,
  ): EitherT[IO, String, Unit] =
    Voter.getRegisteredUserAndChasterID(pattern, registeredUserRepository, event).semiflatMap { (user, sharedLinkId) =>
      Kleisli { (token: UserToken) =>
        for
          sharedLink <- client.sharedLink(sharedLinkId).run(token)
          voteResult  = client.vote(sharedLink.lockId, sharedLink.extensionId, action, sharedLinkId).run(token)
          _          <- voteResult.foldF(
                          _ => user.sendMessage("You can't vote on that lock yet."),
                          duration =>
                            user.sendMessage(
                              s"I've voted on your behalf! Time was ${if duration.toSeconds > 0 then "Added" else "Removed"}",
                            ),
                        )
        yield ()
      }
        .runUsingTokenOf(user)
    }
