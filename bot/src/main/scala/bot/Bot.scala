package bot

import doobie.{Transactor, LogHandler}
import cats.data.{EitherT, NonEmptyList}
import cats.effect.{Deferred, ExitCode, IO, IOApp, Ref, Resource}
import cats.syntax.parallel.*
import cats.syntax.foldable.*
import cats.syntax.traverse.*
import fs2.Stream
import bot.commands
import bot.commands.*
import bot.chaster.{ChasterClient, ChasterConfiguration}
import bot.syntax.all.*
import bot.model.{Channel, Discord, RegisteredUser, Role, User}
import bot.tasks.*
import bot.db.{RegisteredUserRepository, UserTokenRepository, DoobieLogHandler}
import cats.effect.unsafe.IORuntime
import net.dv8tion.jda.api.requests.GatewayIntent
import net.dv8tion.jda.api.{JDA, JDABuilder}
import org.flywaydb.core.Flyway
import org.http4s.ember.client.*
import org.http4s.*
import org.http4s.dsl.io.*
import org.http4s.client.*
import org.http4s.client.middleware.{Retry, RetryPolicy}
import org.http4s.ember.server.EmberServerBuilder
import org.http4s.server.Server
import org.typelevel.ci.*
import org.typelevel.log4cats.{Logger, SelfAwareStructuredLogger}
import org.typelevel.log4cats.slf4j.Slf4jLogger

import java.time.LocalDateTime
import java.time.format.DateTimeFormatter
import java.time.temporal.ChronoUnit
import scala.jdk.CollectionConverters.*
import scala.concurrent.duration.*
import scala.compiletime.uninitialized

object Bot:
  private def httpServer(
    chasterConfiguration: ChasterConfiguration,
    routes: HttpRoutes[IO],
  )(using Logger[IO]): Resource[IO, Server] =
    EmberServerBuilder
      .default[IO]
      .withLogger(summon[Logger[IO]])
      .withHost(chasterConfiguration.host)
      .withPort(chasterConfiguration.port)
      .withHttpApp(routes.orNotFound)
      .build

  private def acquireDiscordClient(
    token: String,
    messageListener: MessageListener,
  )(using Logger[IO]): Resource[IO, Discord] =
    val acquire = IO {
      JDABuilder.createDefault(token)
        .enableIntents(GatewayIntent.GUILD_MEMBERS)
        .addEventListeners(messageListener)
        .build()
        .awaitReady()
    }

    Resource.make(acquire)(jda => IO(jda.shutdown()))
      .map(new Discord(_))
      .evalTap(_ => Logger[IO].info("Loaded JDA"))

  private def runMigrations(postgresConfig: PostgresConfiguration)(using Logger[IO]): IO[Unit] =
    for
      flyway <- IO {
        Flyway
          .configure
          .dataSource(postgresConfig.url, postgresConfig.user, postgresConfig.password)
          .validateMigrationNaming(true)
          .baselineOnMigrate(true)
          .load
      }
      migrations <- IO(flyway.migrate())
      _ <- Logger[IO].debug(s"Ran ${migrations.migrationsExecuted} migrations.")
    yield ()

  private def loadConfigs: IO[(DiscordConfiguration, PostgresConfiguration)] =
    for
      discordConfig <- DiscordConfiguration.fromConfig()
      postgres <- PostgresConfiguration.fromConfig()
    yield (discordConfig, postgres)

  abstract class Builder[A]:
    def apply(
      discord: Deferred[IO, Discord],
      chasterClient: ChasterClient,
      registeredUserRepository: RegisteredUserRepository,
      userTokenRepository: UserTokenRepository,
    )(using Transactor[IO], LogHandler, Logger[IO]): A

  def run[Log <: DiscordLogger](
    commanderBuilder: Builder[Commander[Log]],
  )(using IORuntime): IO[Unit] =
    for
      given Logger[IO] <- Slf4jLogger.create[IO]
      (discordConfig, postgresConfig) <- loadConfigs
      //given DiscordLogger <- DiscordLogger.create

      _ <- runMigrations(postgresConfig)
      given Transactor[IO] = postgresConfig.transactor
      given LogHandler <- DoobieLogHandler.default
      discordDeferred <- Deferred[IO, Discord]

      userTokenRepository = new UserTokenRepository
      registeredUserRepository = new RegisteredUserRepository(discordDeferred, userTokenRepository)

      chasterClient <- ChasterClient(
        (id, accessToken) => userTokenRepository.update(id, accessToken.access_token, accessToken.expiresAt, accessToken.refresh_token, accessToken.scope)
      )

      commander = commanderBuilder(
        discordDeferred,
        chasterClient,
        registeredUserRepository,
        userTokenRepository,
      )

      given Log = commander.logger

      registration <- Registration(registeredUserRepository, userTokenRepository, chasterClient, commander.unregisterHooks)

      commanderWithDefaults = commander.withDefaults(
        registeredUserRepository,
        userTokenRepository,
        registration
      )

      messageListener = new MessageListener(registration, commanderWithDefaults)
      (discord, releaseDiscord) <- acquireDiscordClient(discordConfig.token, messageListener).allocated
      _ <- discordDeferred.complete(discord)
      _ <- commander.onDiscordAcquired(discord)
      (_, releaseHttpServer) <- httpServer(chasterClient.config, registration.routes).allocated
      _ <- commanderWithDefaults.registerSlashCommands(discord).start
      _ <- commanderWithDefaults.combinedTasks.compile.lastOrError
      _ <- releaseDiscord
      _ <- releaseHttpServer
    yield ()
