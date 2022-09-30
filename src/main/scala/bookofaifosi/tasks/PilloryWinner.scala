package bookofaifosi.tasks

import bookofaifosi.Bot
import bookofaifosi.db.{PilloryBitchesRepository, PilloryLinkRepository}
import bookofaifosi.db.Filters.*
import bookofaifosi.model.PilloryBitches
import bookofaifosi.syntax.io.*
import bookofaifosi.syntax.stream.*
import cats.effect.IO
import cats.syntax.traverse.*
import cats.syntax.foldable.*
import cats.syntax.option.*
import fs2.Stream
import org.typelevel.log4cats.Logger
import doobie.syntax.string.*
import doobie.postgres.implicits.*
import scala.concurrent.duration.*
import java.time.temporal.ChronoUnit
import java.time.{Instant, ZoneOffset}

object PilloryWinner extends Streams:
  private def offsetFromConfig: Stream[IO, Unit] =
    val now = Instant.now.atOffset(ZoneOffset.UTC)
    val withHourMinute = now.withSecond(0).withHour(Bot.config.pilloryBitches.hours).withMinute(Bot.config.pilloryBitches.minutes)
    val target = if now.isAfter(withHourMinute) then withHourMinute.plusDays(1) else withHourMinute
    val offset = ChronoUnit.SECONDS.between(now, target).seconds
    IO.sleep(offset).streamed

  override lazy val stream: Stream[IO, Unit] =
    for
      _ <- offsetFromConfig
      _ <- Stream.unit ++ Stream.awakeEvery[IO](24.hours)
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
