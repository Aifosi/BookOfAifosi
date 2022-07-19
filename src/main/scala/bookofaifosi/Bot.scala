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
import net.dv8tion.jda.api.{JDA, JDABuilder}
import org.flywaydb.core.Flyway
import org.http4s.blaze.client.*
import org.http4s.*
import org.http4s.blaze.server.BlazeServerBuilder
import org.http4s.dsl.io.*
import org.http4s.client.*
import org.typelevel.log4cats.{Logger, SelfAwareStructuredLogger}
import org.typelevel.log4cats.slf4j.Slf4jLogger

import scala.jdk.CollectionConverters.*
import scala.concurrent.duration.*

//https://discord.com/oauth2/authorize?client_id=987840268726312970&scope=bot%20applications.commands&permissions=534992186432
//https://discord.com/oauth2/authorize?client_id=990221153203281950&scope=bot%20applications.commands&permissions=534992186432 - Test
//Add view channels permission
object Bot extends IOApp:
  lazy val discordConfig = DiscordConfiguration.fromConfig()
  lazy val chasterConfig = ChasterConfiguration.fromConfig()
  lazy val dbConfig = DatabaseConfiguration.fromConfig()
  lazy val config = Configuration.fromConfig()

  val client: Deferred[IO, Client[IO]] = Deferred.unsafe
  val discord: Deferred[IO, Discord] = Deferred.unsafe
  val logger: Deferred[IO, Logger[IO]] = Deferred.unsafe

  def ioRuntime: IORuntime = runtime
  given IORuntime = runtime

  val xa: Transactor[IO] = Transactor.fromDriverManager[IO](
    dbConfig.driver, dbConfig.url, dbConfig.user, dbConfig.password
  )

  lazy val allCommands: NonEmptyList[AnyCommand] = NonEmptyList.of(
    Help,
    TagAdd,
    TagInfo,
    TagList,
    TagRemove,
    TagUpdate,
    Reminder,
    RegisterWearer,
    RegisterKeyholder,
    Subscribe,
    AddDeadline,
    EnablePilloryBitches,
    DisablePilloryBitches,
    PilloryChecker,
    TriggerMessage,
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
    Subscribe,
    AddDeadline,
    PilloryWinner,
    UpdateWearers,
  )
  
  def tasks(using Logger[IO]): Stream[IO, Unit] = tasks.map(_.stream).reduceLeft(_.concurrently(_))

  def runMigrations(using Logger[IO]): IO[Unit] =
    for
      flyway <- IO(Flyway.configure.dataSource(dbConfig.url, dbConfig.user, dbConfig.password).baselineOnMigrate(true).load)
      migrations <- IO(flyway.migrate())
      _ <- Logger[IO].debug(s"Ran ${migrations.migrationsExecuted} migrations.")
    yield ()

  private def jdaIO(using Logger[IO]): IO[JDA] =
    val jda = JDABuilder.createDefault(discordConfig.token).addEventListeners(new MessageListener)
    IO(jda.build().awaitReady())

  def registerSlashCommands(discord: Discord)(using Logger[IO]): IO[Unit] =
    val patterns = SlashPattern.buildCommands(slashCommands.map(_.pattern))
    discord.guilds.traverse_(_.addCommands(patterns))
      *> Logger[IO].info("All Slash commands registered.")

  def httpServer(using Logger[IO]): Stream[IO, ExitCode] = BlazeServerBuilder[IO]
    .bindHttp(config.port, config.host)
    .withHttpApp(Registration.routes.orNotFound)
    .serve

  override def run(args: List[String]): IO[ExitCode] =
    (for
      logger  <- Slf4jLogger.create[IO].streamed
      given Logger[IO] = logger
      _ <- Bot.logger.complete(logger).streamed
      _ <- runMigrations.streamed
      client <- Stream.resource(BlazeClientBuilder[IO].resource)
      _ <- Bot.client.complete(client).streamed
      _ <- Logger[IO].info("HTTP client acquired.").streamed
      jda <- jdaIO.streamed
      discord = new Discord(jda)
      _ <- discord.jda.updateCommands().toIO.streamed //TODO Remove me - Delete all global commands 
      _ <- Bot.discord.complete(discord).streamed
      _ <- Logger[IO].info("Loaded JDA").streamed
      _ <- registerSlashCommands(discord).start.streamed
      exitCode <- httpServer.concurrently(tasks)
    yield exitCode).compile.lastOrError
