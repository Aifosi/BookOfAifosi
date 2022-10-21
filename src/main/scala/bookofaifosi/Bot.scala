package bookofaifosi

import doobie.util.transactor.Transactor
import cats.data.NonEmptyList
import cats.effect.{Deferred, ExitCode, IO, IOApp, Ref}
import cats.syntax.parallel.*
import cats.syntax.foldable.*
import cats.syntax.traverse.*
import fs2.Stream
import bookofaifosi.commands
import bookofaifosi.commands.*
import bookofaifosi.syntax.all.*
import bookofaifosi.model.{Channel, Discord, Role, User}
import bookofaifosi.tasks.*
import cats.effect.unsafe.IORuntime
import net.dv8tion.jda.api.requests.GatewayIntent
import net.dv8tion.jda.api.{JDA, JDABuilder}
import org.flywaydb.core.Flyway
import org.http4s.blaze.client.*
import org.http4s.{Request, *}
import org.http4s.blaze.server.BlazeServerBuilder
import org.http4s.dsl.io.*
import org.http4s.client.*
import org.http4s.client.middleware.{Retry, RetryPolicy}
import org.typelevel.ci.*
import org.typelevel.log4cats.{Logger, SelfAwareStructuredLogger}
import org.typelevel.log4cats.slf4j.Slf4jLogger

import java.time.LocalDateTime
import java.time.format.DateTimeFormatter
import java.time.temporal.ChronoUnit
import scala.jdk.CollectionConverters.*
import scala.concurrent.duration.*

//https://discord.com/oauth2/authorize?client_id=987840268726312970&scope=bot%20applications.commands&permissions=534992186432
//https://discord.com/oauth2/authorize?client_id=990221153203281950&scope=bot%20applications.commands&permissions=534992186432 - Test
//Add view channels permission
//TODO use streams to list from database
object Bot extends IOApp:
  lazy val discordConfig = DiscordConfiguration.fromConfig()
  lazy val chasterConfig = ChasterConfiguration.fromConfig()
  lazy val postgresConfig = PostgresConfiguration.fromConfig()
  lazy val mysqlConfig = MysqlConfiguration.fromConfig()
  lazy val config = Configuration.fromConfig()

  val client: Deferred[IO, Client[IO]] = Deferred.unsafe
  val discord: Deferred[IO, Discord] = Deferred.unsafe
  val logger: Deferred[IO, Logger[IO]] = Deferred.unsafe

  def ioRuntime: IORuntime = runtime
  given IORuntime = runtime

  val postgresTransactor: Transactor[IO] = Transactor.fromDriverManager[IO](
    postgresConfig.driver, postgresConfig.url, postgresConfig.user, postgresConfig.password
  )

  val mysqlTransactor: Transactor[IO] = Transactor.fromDriverManager[IO](
    mysqlConfig.driver, mysqlConfig.url, mysqlConfig.user, mysqlConfig.password
  )

  lazy val allCommands: NonEmptyList[AnyCommand] = NonEmptyList.of(
    Help,
    Register,
    EnablePilloryBitches,
    DisablePilloryBitches,
    PilloryChecker,
    TriggerMessage,
    MessageDeleter,
    LockChannel,
    UnlockChannel,
    Nuke,
    Task,
    TaskCompleter,
  )

  lazy val textCommands: List[TextCommand] = allCommands.collect {
    case command: TextCommand => command
  }
  lazy val reactionCommands: List[ReactionCommand] = allCommands.collect {
    case command: ReactionCommand => command
  }
  lazy val slashCommands: List[SlashCommand] = allCommands.collect {
    case command: SlashCommand => command
  }
  lazy val autoCompletableCommands: List[AutoCompletable] = allCommands.collect {
    case command: AutoCompletable => command
  }

  lazy val tasks: NonEmptyList[Streams] = NonEmptyList.of(
    PilloryWinner,
    UpdateUsers,
    WheelTasks,
  )

  private def combinedTasks(using Logger[IO]): Stream[IO, Unit] = tasks.map(_.stream.logErrorAndContinue()).reduceLeft(_.concurrently(_))

  private def runMigrations(using Logger[IO]): IO[Unit] =
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

  private def acquireDiscordClient(using Logger[IO]): IO[Discord] =
    val jda = JDABuilder.createDefault(discordConfig.token)
      .enableIntents(GatewayIntent.GUILD_MEMBERS)
      .addEventListeners(new MessageListener)
    for
      jda <- IO(jda.build().awaitReady())
      discord = new Discord(jda)
      _ <- Bot.discord.complete(discord)
      _ <- Logger[IO].info("Loaded JDA")
    yield discord

  private def registerSlashCommands(discord: Discord)(using Logger[IO]): IO[Unit] =
    val patterns = SlashPattern.buildCommands(slashCommands.map(_.pattern))
    discord.guilds.traverse_(_.addCommands(patterns))
      *> Logger[IO].info("All Slash commands registered.")

  private def httpServer(using Logger[IO]): Stream[IO, ExitCode] = BlazeServerBuilder[IO]
    .bindHttp(config.port, config.host)
    .withHttpApp(Registration.routes.orNotFound)
    .serve

  private def acquireHttpClient(using Logger[IO]): Stream[IO, Client[IO]] =
    def retryPolicy(request: Request[IO], response: Either[Throwable, Response[IO]], retries: Int): Option[FiniteDuration] =
      response.toOption.flatMap {
        case response if response.status == Status.TooManyRequests =>
          response.headers.get(ci"x-ratelimit-reset")
            .map(_.head.value)
            .map(LocalDateTime.parse(_, DateTimeFormatter.ofPattern("EEE, dd MMM yyy HH:mm:ss zzz")))
            .map(ChronoUnit.SECONDS.between(LocalDateTime.now, _).seconds)
        case _ => None
      }

    for
      client <- Stream.resource(BlazeClientBuilder[IO].resource)
      clientWithRetry = Retry(retryPolicy)(client)
      _ <- Bot.client.complete(clientWithRetry).streamed
      _ <- Logger[IO].info("HTTP client acquired.").streamed
    yield clientWithRetry

  override def run(args: List[String]): IO[ExitCode] =
    (for
      logger  <- Slf4jLogger.create[IO].streamed
      given Logger[IO] = logger
      _ <- Bot.logger.complete(logger).streamed
      _ <- runMigrations.streamed
      _ <- acquireHttpClient
      discord <- acquireDiscordClient.streamed
      _ <- registerSlashCommands(discord).start.streamed
      exitCode <- httpServer.concurrently(combinedTasks)
    yield exitCode).compile.lastOrError
