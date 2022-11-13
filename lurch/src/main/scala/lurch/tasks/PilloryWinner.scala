package lurch.tasks

import bot.db.Filters.*
import bot.syntax.io.*
import bot.syntax.stream.*
import bot.tasks.Streams
import cats.effect.IO
import cats.syntax.foldable.*
import cats.syntax.option.*
import cats.syntax.traverse.*
import doobie.postgres.implicits.*
import doobie.syntax.string.*
import fs2.Stream
import lurch.Lurch
import lurch.db.{PilloryBitchesRepository, PilloryLinkRepository}
import lurch.model.PilloryBitches
import org.typelevel.log4cats.Logger

import java.time.temporal.ChronoUnit
import java.time.{Instant, ZoneOffset}
import scala.concurrent.duration.*

object PilloryWinner extends Streams:
  private def offsetFromConfig: Stream[IO, Unit] =
    val now = Instant.now.atOffset(ZoneOffset.UTC)
    val withHourMinute = now.withSecond(0).withHour(Lurch.config.pilloryBitches.hours).withMinute(Lurch.config.pilloryBitches.minutes)
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
