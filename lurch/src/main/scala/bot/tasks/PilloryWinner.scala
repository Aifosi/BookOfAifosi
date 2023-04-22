package bot.tasks

import bot.Configuration
import bot.db.{PilloryBitchesRepository, PilloryLinkRepository}
import bot.db.Filters.*
import bot.model.PilloryBitches
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
import java.time.{Instant, ZoneOffset}
import java.time.temporal.ChronoUnit
import org.typelevel.log4cats.Logger
import scala.concurrent.duration.*

class PilloryWinner(
  pilloryBitchesRepository: PilloryBitchesRepository,
  pilloryLinkRepository: PilloryLinkRepository,
  config: Configuration,
) extends Streams:
  private def offsetFromConfig: Stream[IO, Unit] =
    val now            = Instant.now.atOffset(ZoneOffset.UTC)
    val withHourMinute =
      now.withSecond(0).withHour(config.pilloryBitches.hours).withMinute(config.pilloryBitches.minutes)
    val target         = if now.isAfter(withHourMinute) then withHourMinute.plusDays(1) else withHourMinute
    val offset         = ChronoUnit.SECONDS.between(now, target).seconds
    IO.sleep(offset).streamed

  override lazy val stream: Stream[IO, Unit] =
    for
      _                              <- offsetFromConfig
      _                              <- Stream.unit ++ Stream.awakeEvery[IO](24.hours)
      PilloryBitches(guild, channel) <- Stream.evalSeq(pilloryBitchesRepository.list())
      // timeFilter = fr"created_at >= ${Instant.now.minus(Bot.config.pilloryBitchesFrequency.toMinutes + 1, ChronoUnit.MINUTES)}".some
      notCountedFilter                = fr"counted = FALSE".some
      pilloryLinks                   <- pilloryLinkRepository.list(guild.discordID.equalGuildID, notCountedFilter).streamed
      userVotes                       = pilloryLinks.groupBy(_.user).view.mapValues(_.length).toList
      winnerMessage                   = userVotes.maxByOption(_._2).fold("No winners today.") { case (_, submissions) =>
                                          val winners = userVotes.collect { case (winner, `submissions`) =>
                                            winner
                                          }
                                          s"Congratulations ${winners.map(_.mention).mkString(", ")}! You win Pillory Bitches today."
                                        }
      _                              <- pilloryLinkRepository.setCounted(guild.discordID).streamed
      _                              <- channel.sendMessage(winnerMessage).streamed
    yield ()
