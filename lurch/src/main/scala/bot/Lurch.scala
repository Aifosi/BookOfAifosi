package bot

import bot.Bot.Builder
import bot.chaster.ChasterClient
import bot.commands.*
import bot.db.*
import bot.db.{RegisteredUserRepository, UserTokenRepository}
import bot.db.Filters.*
import bot.db.RecentLockHistoryRepository
import bot.model.Discord
import bot.tasks.*
import bot.tasks.{Streams, WheelCommand}
import bot.tasks.WheelCommands
import bot.wheel.*
import bot.wheel.{Task as WheelTask, *}

import cats.data.{EitherT, NonEmptyList}
import cats.effect.{Deferred, ExitCode, IO, IOApp}
import cats.effect.unsafe.IORuntime
import doobie.{LogHandler, Transactor}
import fs2.Stream
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter
import java.time.temporal.ChronoUnit
import net.dv8tion.jda.api.JDABuilder
import net.dv8tion.jda.api.requests.GatewayIntent
import org.flywaydb.core.Flyway
import org.http4s.{Request, Response, Status}
import org.http4s.client.Client
import org.http4s.client.middleware.Retry
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger
import scala.concurrent.duration.FiniteDuration

//https://discord.com/oauth2/authorize?client_id=987840268726312970&scope=bot%20applications.commands&permissions=534992186432
//https://discord.com/oauth2/authorize?client_id=990221153203281950&scope=bot%20applications.commands&permissions=534992186432 - Test
//Add view channels permission
//TODO use streams to list from database
object Lurch extends IOApp.Simple:
  private def commanderBuilder(
    config: Configuration,
    mysqlConfiguration: MysqlConfiguration,
    channelConfiguration: ChannelConfiguration,
    lurchChannelConfiguration: LurchChannelConfiguration,
  )(using lurchLogger: LurchLogger) = new Builder[Commander[LurchLogger]]:
    override def apply(
      discord: Deferred[IO, Discord],
      chasterClient: ChasterClient,
      registeredUserRepository: RegisteredUserRepository,
      userTokenRepository: UserTokenRepository,
    )(using Transactor[IO], LogHandler, Logger[IO]): Commander[LurchLogger] =
      val recentLockHistoryRepository = new RecentLockHistoryRepository(registeredUserRepository)
      val lockedChannelsRepository    = new LockedChannelsRepository(discord)
      val pendingTaskRepository       = new PendingTaskRepository(registeredUserRepository)
      val pilloryBitchesRepository    = new PilloryBitchesRepository(discord)
      val pilloryLinkRepository       = new PilloryLinkRepository(discord, registeredUserRepository)

      val commands: List[AnyCommand] = List(
        new EnablePilloryBitches(pilloryBitchesRepository),
        new DisablePilloryBitches(pilloryBitchesRepository),
        new PilloryChecker(chasterClient, registeredUserRepository, pilloryLinkRepository, pilloryBitchesRepository),
        TriggerMessage,
        new MessageDeleter(lockedChannelsRepository),
        new LockChannel(lockedChannelsRepository),
        new UnlockChannel(lockedChannelsRepository),
        new TaskCommand(registeredUserRepository, mysqlConfiguration.transactor),
        new TaskCompleter(pendingTaskRepository, lurchLogger.tortureChamberChannelDeferred),
      )

      val wheelCommands: NonEmptyList[WheelCommand[?]] =
        commonWheelCommands(chasterClient, registeredUserRepository) :+
          new wheel.Task(chasterClient, registeredUserRepository, pendingTaskRepository, mysqlConfiguration.transactor)

      val tasks: NonEmptyList[Streams] = NonEmptyList.of(
        new PilloryWinner(pilloryBitchesRepository, pilloryLinkRepository, config),
        new UpdateUsers(discord, chasterClient, registeredUserRepository, config),
        new WheelCommands(
          chasterClient,
          registeredUserRepository,
          recentLockHistoryRepository,
          wheelCommands,
          config.checkFrequency,
        ),
      )

      Commander(
        lurchLogger,
        commands,
        tasks,
        lurchLogger.complete(_, channelConfiguration, lurchChannelConfiguration),
        Set(registeredUser => EitherT.liftF(pendingTaskRepository.remove(registeredUser.id.equalID).void)),
      )

  override def run: IO[Unit] =
    for
      config                     <- Configuration.fromConfig()
      mysqlConfiguration         <- MysqlConfiguration.fromConfig()
      commonChannelConfiguration <- ChannelConfiguration.fromConfig()
      channelConfiguration       <- LurchChannelConfiguration.fromConfig()
      given LurchLogger          <- LurchLogger.create
      _                          <- Bot.run(commanderBuilder(config, mysqlConfiguration, commonChannelConfiguration, channelConfiguration))(using
                                      runtime,
                                    )
    yield ()
