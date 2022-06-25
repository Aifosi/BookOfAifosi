package bookofaifosi

import bookofaifosi.Bot.logger
import doobie.util.transactor.Transactor
import cats.data.NonEmptyList
import cats.effect.{Deferred, ExitCode, IO, IOApp, Ref}
import cats.syntax.parallel.*
import cats.syntax.foldable.*
import cats.syntax.traverse.*
import fs2.Stream
import bookofaifosi.commands.*
import bookofaifosi.commands
import bookofaifosi.syntax.all.*
import bookofaifosi.model.{Channel, Discord, Role, User}
import cats.effect.unsafe.IORuntime
import net.dv8tion.jda.api.{JDA, JDABuilder}
import org.flywaydb.core.Flyway
import org.http4s.blaze.client.*
import org.http4s.*
import org.http4s.blaze.server.BlazeServerBuilder
import org.http4s.dsl.io.*
import org.http4s.client.*
import org.typelevel.log4cats.SelfAwareStructuredLogger
import org.typelevel.log4cats.slf4j.Slf4jLogger

import scala.concurrent.duration.*

//https://discord.com/oauth2/authorize?client_id=987840268726312970&scope=bot&permissions=534992185408
//https://discord.com/oauth2/authorize?client_id=990221153203281950&scope=bot&permissions=534992185408 - Test
object Bot extends IOApp:
  lazy val discordConfig = DiscordConfiguration.fromConfig()
  lazy val chasterConfig = ChasterConfiguration.fromConfig()
  lazy val dbConfig = DatabaseConfiguration.fromConfig()
  lazy val config = Configuration.fromConfig()

  val client: Deferred[IO, Client[IO]] = Deferred.unsafe
  val discord: Deferred[IO, Discord] = Deferred.unsafe
  val logger: Deferred[IO, SelfAwareStructuredLogger[IO]] = Deferred.unsafe

  def ioRuntime: IORuntime = runtime
  
  val xa: Transactor[IO] = Transactor.fromDriverManager[IO](
    dbConfig.driver, dbConfig.url, dbConfig.user, dbConfig.password
  )

  val runMigrations: IO[Unit] =
    for
      flyway <- IO(Flyway.configure.dataSource(dbConfig.url, dbConfig.user, dbConfig.password).baselineOnMigrate(true).load)
      migrations <- IO(flyway.migrate())
      _ <- logger.debug(s"Ran ${migrations.migrationsExecuted} migrations.")
    yield ()

  val allCommands: NonEmptyList[AnyCommand] = NonEmptyList.of(
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
    commands.AddDeadline,
    RoleSetWearer,
    RoleSetKeyholder,
    EnablePilloryBitches,
    DisablePilloryBitches,
    PilloryChecker,
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
  lazy val commandStreams: Stream[IO, Unit] = allCommands.collect {
    case command: Streams => command.stream.logErrorAndContinue
  }.foldLeft(Stream.never[IO])(_.concurrently(_))

  private val jdaIO: IO[JDA] =
    val jda = JDABuilder.createDefault(discordConfig.token).addEventListeners(MessageListener)
    IO(jda.build().awaitReady())

  def registerSlashCommands(jda: JDA): IO[Unit] =
    //jda.deleteCommandById(989318936577310770L).toIO *>
    SlashPattern.buildCommands(slashCommands.map(_.pattern)).parTraverse_ { data =>
      jda.upsertCommand(data).toIO
    } *> Bot.logger.info("All Slash commands registered.")

  val httpServer: Stream[IO, ExitCode] = BlazeServerBuilder[IO]
    .bindHttp(config.port, config.host)
    .withHttpApp(Registration.routes.orNotFound)
    .serve

  override def run(args: List[String]): IO[ExitCode] =
    (for
      logger <- Slf4jLogger.create[IO].streamed
      _ <- Bot.logger.complete(logger).streamed
      client <- Stream.resource(BlazeClientBuilder[IO].resource)
      _ <- Bot.client.complete(client).streamed
      _ <- logger.info("HTTP client acquired.").streamed
      _ <- runMigrations.streamed
      jda <- jdaIO.streamed
      _ <- Bot.discord.complete(new Discord(jda)).streamed
      _ <- logger.info("Loaded JDA").streamed
      _ <- registerSlashCommands(jda).start.streamed
      exitCode <- httpServer.concurrently(commandStreams)
    yield exitCode).compile.lastOrError
