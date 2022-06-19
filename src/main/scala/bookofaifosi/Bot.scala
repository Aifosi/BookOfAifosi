package bookofaifosi

import doobie.util.transactor.Transactor
import cats.data.NonEmptyList
import cats.effect.{Deferred, ExitCode, IO, IOApp, Ref}
import cats.syntax.parallel.*
import cats.syntax.foldable.*
import fs2.Stream
import bookofaifosi.commands.*
import bookofaifosi.syntax.all.*
import bookofaifosi.wrappers.{Channel, Role, User}
import net.dv8tion.jda.api.{JDA, JDABuilder}
import org.flywaydb.core.Flyway

import scala.concurrent.duration.*

//https://discord.com/api/oauth2/authorize?client_id=987840268726312970&permissions=139586750528&scope=bot
//https://discord.com/oauth2/authorize?client_id=987840268726312970&scope=bot&permissions=377986731072
object Bot extends IOApp:
  lazy val config = Configuration.fromConfig()
  lazy val dbConfig = DatabaseConfiguration.fromConfig()

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
    AddTag,
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

  private val jdaIO: IO[JDA] =
    val jda = JDABuilder.createDefault(config.token).addEventListeners(MessageListener)
    IO(jda.build().awaitReady())

  def registerSlashCommands(jda: JDA): IO[Unit] =
    slashCommands.map { command =>
      val data = command.pattern.build.setDefaultEnabled(command.defaultEnabled)

      for
        _ <- IO.println(s"""Registering "${data.getName}", enabled? ${data.isDefaultEnabled}""")
        _ <- jda.upsertCommand(data).toIO
        //_ <- jda.getGuildById(987807096332505118L).upsertCommand(data).toIO
      yield ()
    }.sequence_

  /*override def run: IO[Unit] =
    (for
      _ <- Stream.eval(runMigrations)
      jda <- Stream.eval(jdaIO)
      _ <- Stream.eval(registerSlashCommands(jda))
    yield ()).compile.drain*/

  override def run(args: List[String]): IO[ExitCode] =
    (for
      jda <- Stream.eval(jdaIO)
      _ <- Stream.eval(registerSlashCommands(jda))
      _ <- Stream.never[IO]
    yield ()).compile.drain.as(ExitCode.Success)