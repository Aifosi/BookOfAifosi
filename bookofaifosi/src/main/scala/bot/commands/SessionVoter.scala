package bot.commands

import bot.chaster.{ChasterClient, SharedLink, VoteAction}
import bot.commands.{Hidden, NoLog, TextCommand}
import bot.db.Filters.*
import bot.db.{RegisteredUserRepository, given}
import bot.model.{ChasterID, *}
import bot.model.event.{MessageEvent, ReactionEvent}
import bot.syntax.io.*
import bot.syntax.stream.*
import bot.tasks.Streams
import cats.data.{EitherT, OptionT}
import cats.effect.IO
import cats.syntax.foldable.*
import cats.syntax.option.*
import cats.syntax.traverse.*
import doobie.postgres.implicits.*
import doobie.syntax.string.*
import fs2.Stream
import org.typelevel.log4cats.Logger

import java.time.temporal.ChronoUnit
import java.time.{Instant, ZoneOffset}
import scala.concurrent.duration.*
import scala.util.chaining.*
import scala.util.matching.Regex

object SessionVoter extends TextCommand with Hidden with NoLog:
  val add = "⏫"
  val remove = "⏬"
  val random = "\uD83D\uDD00"

  override val pattern: Regex = ".*http(?:s)?://chaster.app/sessions/(\\w{16}).*".r

  override def apply(pattern: Regex, event: MessageEvent)(using Logger[IO]): IO[Boolean] =
    for
      _ <- event.message.addReaction(add)
      _ <- event.message.addReaction(remove)
      _ <- event.message.addReaction(random)
    yield true

  private def getRegisteredUserAndSharedLink(
    registeredUserRepository: RegisteredUserRepository,
    event: ReactionEvent,
  ): EitherT[IO, String, (RegisteredUser, ChasterID)] = for
    message <- EitherT.liftF(event.message)
    sharedLinkId <- message.content match
      case pattern(sharedLinkId) => EitherT.pure[IO, String](ChasterID(sharedLinkId))
      case _                     => EitherT.leftT[IO, ChasterID](s"Could no extract shared link from $message")
    author <- EitherT.liftF(event.authorMember)
    user <- registeredUserRepository.find(author.discordID.equalDiscordID)
      .toRight(s"Could not find registered used ${event.author}")
  yield (user, sharedLinkId)

  def vote(
    chasterClient: ChasterClient,
    registeredUserRepository: RegisteredUserRepository,
    event: ReactionEvent,
    action: VoteAction
  ): EitherT[IO, String, Unit] =
    getRegisteredUserAndSharedLink(registeredUserRepository, event).semiflatMap { (user, sharedLinkId) =>
      val authenticatedEndpoints = chasterClient.authenticatedEndpoints(user.token)
      def voteAndNotify(sharedLink: SharedLink) = for
        duration <- authenticatedEndpoints.vote(sharedLink.lockId, sharedLink.extensionId, action, sharedLinkId)
        _ <- user.sendMessage(s"I've voted on your behalf! Time was ${if duration.toSeconds > 0 then "Added" else "Removed"}")
      yield ()

      for
        sharedLink <- authenticatedEndpoints.sharedLink(sharedLinkId)
        _ <- if sharedLink.canVote then voteAndNotify(sharedLink) else user.sendMessage("You can't vote on that lock yet.")
      yield ()
    }
