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
import org.http4s.blaze.client.*
import org.http4s.client.*

import scala.concurrent.duration.*

//https://discord.com/api/oauth2/authorize?client_id=987840268726312970&permissions=139586750528&scope=bot
//https://discord.com/oauth2/authorize?client_id=987840268726312970&scope=bot&permissions=377986731072
object Bot extends IOApp.Simple:
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
    TagAdd,
    TagInfo,
    TagList,
    TagRemove,
    TagUpdate,
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
    slashCommands.groupBy(_.command).view.mapValues { commands =>
      val pattern = commands.map(_.pattern).reduceLeft(_.merge(_))
      val data = pattern.build.setDefaultEnabled(commands.forall(_.defaultEnabled))
      val subCommandsMessage =
        val subCommands = commands.flatMap(_.subCommand)
        if subCommands.isEmpty then "" else s" [${subCommands.mkString(", ")}]"
      for
        _ <- IO.println(s"""Registering "${data.getName}$subCommandsMessage", enabled? ${data.isDefaultEnabled}""")
        _ <- jda.upsertCommand(data).toIO
      yield ()
    }.toMap.values.toList.sequence_

  val client: Deferred[IO, Client[IO]] = Deferred.unsafe

  override def run: IO[Unit] =
    (for
      client <- Stream.resource(BlazeClientBuilder[IO].resource)
      _ <- Stream.eval(Bot.client.complete(client))
      _ <- Stream.eval(runMigrations)
      jda <- Stream.eval(jdaIO)
      _ <- Stream.eval(registerSlashCommands(jda))
      _ <- Stream.never[IO]
    yield ()).compile.drain