package bot.commands

import bot.chaster.{ChasterClient, Post}
import bot.commands.{Hidden, NoLog, TextCommand}
import bot.db.Filters.*
import bot.db.{RegisteredUserRepository, given}
import bot.model.event.MessageEvent
import bot.model.{*, given}
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
import bot.db.{PilloryBitchesRepository, PilloryLinkRepository}
import bot.model.PilloryBitches
import org.typelevel.log4cats.Logger

import java.time.temporal.ChronoUnit
import java.time.{Instant, ZoneOffset}
import scala.concurrent.duration.*
import scala.util.chaining.*
import scala.util.matching.Regex

class PilloryChecker(
  chasterClient: ChasterClient,
  registeredUserRepository: RegisteredUserRepository,
  pilloryLinkRepository: PilloryLinkRepository,
  pilloryBitchesRepository: PilloryBitchesRepository,
) extends TextCommand with Hidden with NoLog:
  override val pattern: Regex = ".*http(?:s)?://chaster.app/activity/(\\w{24}).*".r

  private def validatePost(post: Post, member: Member): EitherT[IO, String, RegisteredUser] =
    for
      user <- registeredUserRepository.find(member.discordID.equalDiscordID).toRight("You must be registered to submit pillories.")
      notTooOld = post.data.voteEndsAt.isAfter(Instant.now.minus(1, ChronoUnit.DAYS))
      _ <- EitherT.cond(notTooOld, (), "Pillory is too old, must not be older than 1 day.")
      keyholderIsRegistered <- EitherT.liftF(post.lock.keyholder.fold(IO.pure(false)) { keyholder =>
        registeredUserRepository.find(keyholder._id.equalChasterID).isDefined
      })
      _ <- EitherT.cond(keyholderIsRegistered, (), "Your keyholder must be registered.")
      userSubmitted = user.chasterID == post.user._id
      votingEnded = post.data.voteEndsAt.isBefore(Instant.now)
      _ <- EitherT.cond((userSubmitted && votingEnded) || (!userSubmitted && !votingEnded), (), "You must either submit one of your pillories after it ended or someone else's before it ends.")
      alreadySubmitted <- EitherT.liftF(pilloryLinkRepository.find(user.id.equalUserID, fr"post_id = ${post._id}".some).isDefined)
      _ <- EitherT.cond(!alreadySubmitted, (), "That pillory was already submitted.")
    yield user

  private def addReaction(post: Post, member: Member, channel: Channel, event: MessageEvent): IO[Unit] =
    if channel.discordID != event.channel.discordID then
      event.message.addReaction("\uD83D\uDEAB")
    else
      validatePost(post, member).foldF(
        failReason => event.message.addReaction("❌") *> member.sendMessage(failReason),
        user => event.message.addReaction("✅") *> event.guild.flatMap(guild => pilloryLinkRepository.add(user.id, guild.discordID, post._id))
      ).void

  override def apply(pattern: Regex, event: MessageEvent)(using Logger[IO]): IO[Boolean] =
    (for
      member <- OptionT.liftF(event.authorMember)
      id <- OptionT.fromOption(pattern.findFirstMatchIn(event.content).map(_.group(1))).map(ChasterID(_))
      guild <- OptionT.liftF(event.guild)
      PilloryBitches(_, channel) <- pilloryBitchesRepository.find(guild.discordID.equalGuildID)
      post <- OptionT(chasterClient.authenticatedEndpoints(UserToken.empty).post(id).logErrorOption)
      _ <- OptionT.liftF(addReaction(post, member, channel, event))
    yield true).getOrElse(true)