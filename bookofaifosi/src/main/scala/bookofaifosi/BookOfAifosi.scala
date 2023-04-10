package bookofaifosi

import bot.*
import bot.Bot.Builder
import bot.chaster.ChasterClient
import bot.commands.*
import bot.model.Discord
import bot.tasks.{Streams, WheelCommand}
import bot.db.Filters.*
import bot.db.{RegisteredUserRepository, UserTokenRepository}
import cats.data.{EitherT, NonEmptyList}
import cats.effect.unsafe.IORuntime
import cats.effect.{Deferred, ExitCode, IO, IOApp}
import doobie.LogHandler
import doobie.util.transactor.Transactor
import fs2.Stream
import net.dv8tion.jda.api.JDABuilder
import net.dv8tion.jda.api.requests.GatewayIntent
import org.flywaydb.core.Flyway
import org.http4s.{Request, Response, Status}
import org.http4s.client.Client
import org.http4s.client.middleware.Retry
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger
import shared.db.RecentLockHistoryRepository
import shared.tasks.WheelCommands
import shared.wheel.*

import java.time.LocalDateTime
import java.time.format.DateTimeFormatter
import java.time.temporal.ChronoUnit
import scala.concurrent.duration.FiniteDuration

//https://discord.com/oauth2/authorize?client_id=987840268726312970&scope=bot%20applications.commands&permissions=534992186432
//https://discord.com/oauth2/authorize?client_id=990221153203281950&scope=bot%20applications.commands&permissions=534992186432 - Test
//Add view channels permission
//TODO use streams to list from database
object BookOfAifosi extends IOApp.Simple:
  private def commanderBuilder(
    config: Configuration,
    channelConfiguration: ChannelConfiguration,
  )(using discordLogger: DiscordLogger) = new Builder[Commander[DiscordLogger]]:
    override def apply(
      discord: Deferred[IO, Discord],
      chasterClient: ChasterClient,
      registeredUserRepository: RegisteredUserRepository,
      userTokenRepository: UserTokenRepository,
    )(using Transactor[IO], LogHandler, Logger[IO]): Commander[DiscordLogger] =
      val recentLockHistoryRepository = new RecentLockHistoryRepository(registeredUserRepository)

      val wheelCommands: NonEmptyList[WheelCommand] = NonEmptyList.of(
        new Once(chasterClient, registeredUserRepository),
        new OnceGroup(chasterClient, registeredUserRepository),
        //These two need to be before other commands
        new DiceMultiplier(chasterClient, registeredUserRepository),
        new VerificationPictures(chasterClient, registeredUserRepository),
        new PilloryVoteTime(chasterClient, registeredUserRepository),
        new DiceRolls(chasterClient, registeredUserRepository),
        new WheelRolls(chasterClient, registeredUserRepository),
        new VoteTarget(chasterClient, registeredUserRepository),
        new VoteAdd(chasterClient, registeredUserRepository),
        new VoteRemove(chasterClient, registeredUserRepository),
        new AddSegments(chasterClient, registeredUserRepository),
      )

      val tasks: NonEmptyList[Streams] = NonEmptyList.of(
        new WheelCommands(chasterClient, registeredUserRepository, recentLockHistoryRepository, wheelCommands, config.checkFrequency),
      )

      Commander(discordLogger, List.empty, tasks, discordLogger.complete(_, channelConfiguration))

  override def run: IO[Unit] =
    for
      config <- Configuration.fromConfig()
      channels <- ChannelConfiguration.fromConfig()
      given DiscordLogger <- DiscordLogger.create
      _ <- Bot.run(commanderBuilder(config, channels))(using runtime)
    yield ()
