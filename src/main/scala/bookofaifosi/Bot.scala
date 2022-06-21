package bookofaifosi

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
import bookofaifosi.model.{Channel, Role, User, Discord}
import net.dv8tion.jda.api.{JDA, JDABuilder}
import org.flywaydb.core.Flyway
import org.http4s.blaze.client.*
import org.http4s.*
import org.http4s.blaze.server.BlazeServerBuilder
import org.http4s.dsl.io.*
import org.http4s.client.*

import scala.concurrent.duration.*

//https://discord.com/api/oauth2/authorize?client_id=987840268726312970&permissions=139586750528&scope=bot
//https://discord.com/oauth2/authorize?client_id=987840268726312970&scope=bot&permissions=377986731072
object Bot extends IOApp:
  lazy val chasterConfig = ChasterConfiguration.fromConfig()
  lazy val dbConfig = DatabaseConfiguration.fromConfig()
  lazy val config = Configuration.fromConfig()

  val xa: Transactor[IO] = Transactor.fromDriverManager[IO](
    dbConfig.driver, dbConfig.url, dbConfig.user, dbConfig.password
  )

  val runMigrations: IO[Unit] =
    for
      flyway <- IO(Flyway.configure.dataSource(dbConfig.url, dbConfig.user, dbConfig.password).baselineOnMigrate(true).load)
      migrations <- IO(flyway.migrate())
      _ <- IO.println(s"Ran ${migrations.migrationsExecuted} migrations.")
    yield ()

  val allCommands: NonEmptyList[AnyCommand] = NonEmptyList.of(
    Help,
    TagAdd,
    TagInfo,
    TagList,
    TagRemove,
    TagUpdate,
    Reminder,
    Register,
    Subscribe,
    commands.Deadline,
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
    case command: Streams => command.stream
  }.foldLeft(Stream.never[IO])(_.concurrently(_))

  private val jdaIO: IO[JDA] =
    val jda = JDABuilder.createDefault(chasterConfig.token).addEventListeners(MessageListener)
    IO(jda.build().awaitReady())

  def registerSlashCommands(jda: JDA): IO[Unit] =
    SlashPattern.buildCommands(slashCommands.map(_.pattern)).traverse_ { data =>
      jda.upsertCommand(data).toIO
    }

  val client: Deferred[IO, Client[IO]] = Deferred.unsafe
  val discord: Deferred[IO, Discord] = Deferred.unsafe

  override def run(args: List[String]): IO[ExitCode] =
    (for
      client <- Stream.resource(BlazeClientBuilder[IO].resource)
      _ <- Stream.eval(Bot.client.complete(client))
      _ <- Stream.eval(runMigrations)
      jda <- Stream.eval(jdaIO)
      _ <- Stream.eval(Bot.discord.complete(new Discord(jda)))
      _ <- Stream.eval(registerSlashCommands(jda))
      exitCode <- BlazeServerBuilder[IO]
        .bindHttp(config.port, config.host)
        .withHttpApp(Registration.routes.orNotFound)
        .serve
        .concurrently(commandStreams)
    yield exitCode).compile.lastOrError
