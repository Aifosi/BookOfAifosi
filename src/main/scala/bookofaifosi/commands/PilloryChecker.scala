package bookofaifosi.commands
import bookofaifosi.Bot
import bookofaifosi.db.{PilloryBitchesRepository, PilloryLinkRepository, RegisteredUserRepository}
import bookofaifosi.db.Filters.*
import bookofaifosi.db.given
import bookofaifosi.model.event.MessageEvent
import bookofaifosi.model.{Channel, DiscordID, PilloryBitches, RegisteredUser}
import cats.effect.IO
import bookofaifosi.chaster.Client.*
import bookofaifosi.chaster.Post
import bookofaifosi.syntax.io.*
import bookofaifosi.syntax.stream.*
import cats.data.{EitherT, OptionT}
import cats.syntax.traverse.*
import cats.syntax.foldable.*
import cats.syntax.option.*
import doobie.syntax.string.*
import doobie.postgres.implicits.*

import java.time.Instant
import scala.util.matching.Regex
import scala.util.chaining.*
import fs2.Stream

import java.time.temporal.ChronoUnit
import scala.concurrent.duration.*

object PilloryChecker extends TextCommand with Streams:
  override val pattern: Regex = ".*http(?:s)?://chaster.app/activity/(\\w{24}).*".r

  override lazy val stream: Stream[IO, Unit] =
    for
      _ <- Stream.awakeEvery[IO](Bot.config.pilloryBitchesFrequency)
      PilloryBitches(guild, channel) <- Stream.evalSeq(PilloryBitchesRepository.list())
      //timeFilter = fr"created_at >= ${Instant.now.minus(Bot.config.pilloryBitchesFrequency.toMinutes + 1, ChronoUnit.MINUTES)}".some
      notCountedFilter = fr"counted = FALSE".some
      pilloryLinks <- PilloryLinkRepository.list(guild.discordID.equalGuildID, notCountedFilter).streamed
      userVotes = pilloryLinks.groupBy(_.user).view.mapValues(_.length).toList
      winnerMessage = userVotes.maxByOption(_._2).fold("No winners today.") { case (_, submissions) =>
        val winners = userVotes.collect {
          case (winner, `submissions`) => winner
        }
        s"Congratulations ${winners.map(_.mention).mkString(", ")}! You win Pillory Bitches today."
      }
      _ <- PilloryLinkRepository.setCounted(guild.discordID).streamed
      _ <- channel.sendMessage(winnerMessage).streamed
    yield ()

  private def validatePost(post: Post, user: RegisteredUser): IO[Boolean] =
    val userSubmitted = post.user.username == user.chasterName
    val votingEnded = post.data.voteEndsAt.isBefore(Instant.now)
    val notTooOld = post.data.voteEndsAt.isAfter(Instant.now.minus(1, ChronoUnit.DAYS))
    if notTooOld && ((userSubmitted && votingEnded) || (!userSubmitted && !votingEnded)) then
      for
        alreadySubmitted <- PilloryLinkRepository.find(user.id.equalUserID, fr"post_id = ${post._id}".some).map(_.isDefined)
        keyholderIsRegistered <- post.lock.keyholder.fold(IO.pure(false)) { keyholder =>
          RegisteredUserRepository.find(keyholder.username.equalChasterName).map(_.isDefined)
        }
      yield !alreadySubmitted && keyholderIsRegistered
    else
      IO.pure(false)

  private def addReaction(post: Post, user: RegisteredUser, channel: Channel, event: MessageEvent): IO[Unit] =
    if channel.discordID != event.channel.discordID then
      event.message.addReaction("\uD83D\uDEAB")
    else
      for
        valid <- validatePost(post, user)
        _ <- if valid then
          event.message.addReaction("✅") *> PilloryLinkRepository.add(user.id, event.guild.get.discordID, post._id)
        else
          event.message.addReaction("❌")
      yield ()

  override def apply(pattern: Regex, event: MessageEvent): IO[Boolean] =
    (for
      member <- OptionT.liftF(event.authorMember)
      id <- OptionT.fromOption(pattern.findFirstMatchIn(event.content).map(_.group(1)))
      user <- OptionT(RegisteredUserRepository.find(member.discordID.equalUserID))
      PilloryBitches(_, channel) <- OptionT(PilloryBitchesRepository.find(event.guild.get.discordID.equalGuildID))
      post <- OptionT(user.post(id).attempt.map(_.toOption))
      _ <- OptionT.liftF(addReaction(post, user, channel, event))
    yield true).getOrElse(true)

  override val description: String = "Validates pillory links"
