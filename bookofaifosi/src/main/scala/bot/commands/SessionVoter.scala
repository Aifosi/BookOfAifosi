package bot.commands

import bot.chaster.{ChasterClient, VoteAction}
import bot.commands.{Hidden, NoLog, TextCommand}
import bot.db.Filters.*
import bot.db.{RegisteredUserRepository, given}
import bot.model.*
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
  val add = "U+23EB"
  val remove = "U+23EC"
  val random = "U+1F500"

  override val pattern: Regex = ".*http(?:s)?://chaster.app/sessions/(\\w{16}).*".r

  override def apply(pattern: Regex, event: MessageEvent)(using Logger[IO]): IO[Boolean] =
    for
      _ <- event.message.addReaction(add)
      _ <- event.message.addReaction(remove)
      _ <- event.message.addReaction(random)
    yield true

  def vote(
    chasterClient: ChasterClient,
    registeredUserRepository: RegisteredUserRepository,
    event: ReactionEvent,
    action: VoteAction
  ): OptionT[IO, Unit] = for
    message <- OptionT.liftF(event.message)
    sharedLinkId <- message.content match
      case pattern(sharedLinkId) => OptionT.pure[IO](ChasterID(sharedLinkId))
      case _                     => OptionT.none[IO, ChasterID]
    guild <- OptionT.liftF(event.guild)
    user <- registeredUserRepository.find(event.author.discordID.equalDiscordID, guild.discordID.equalGuildID)
    authenticatedEndpoints = chasterClient.authenticatedEndpoints(user.token)
    sharedLink <- OptionT.liftF(authenticatedEndpoints.sharedLink(sharedLinkId))
    _ <- OptionT.liftF(authenticatedEndpoints.vote(sharedLink.lockId, sharedLink.extensionId, action, sharedLinkId))
  yield ()
