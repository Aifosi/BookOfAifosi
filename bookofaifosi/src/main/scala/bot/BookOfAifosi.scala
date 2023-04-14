package bot

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
import bot.db.RecentLockHistoryRepository
import bot.tasks.WheelCommands
import bot.wheel.*

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

      val commands: List[AnyCommand] = List(
        SessionVoter,
        new SessionVoteAdd(chasterClient, registeredUserRepository),
        new SessionVoteRemove(chasterClient, registeredUserRepository),
        new SessionVoteRandom(chasterClient, registeredUserRepository),
      )

      val tasks: NonEmptyList[Streams] = NonEmptyList.of(
        new WheelCommands(
          chasterClient,
          registeredUserRepository,
          recentLockHistoryRepository,
          commonWheelCommands(chasterClient, registeredUserRepository),
          config.checkFrequency,
        ),
      )

      Commander(discordLogger, commands, tasks, discordLogger.complete(_, channelConfiguration))

  override def run: IO[Unit] =
    for
      config <- Configuration.fromConfig()
      channels <- ChannelConfiguration.fromConfig()
      given DiscordLogger <- DiscordLogger.create
      _ <- Bot.run(commanderBuilder(config, channels))(using runtime)
    yield ()
