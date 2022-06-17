package bookofaifosi

import cats.data.NonEmptyList
import cats.effect.{Deferred, ExitCode, IO, IOApp, Ref}
import cats.syntax.parallel.*
import cats.syntax.foldable.*
import fs2.Stream
import bookofaifosi.commands.*
import bookofaifosi.syntax.all.*
import bookofaifosi.wrappers.{Channel, Role, User}
import net.dv8tion.jda.api.{JDA, JDABuilder}

import scala.concurrent.duration.*

object Bot extends IOApp.Simple:
  lazy val config = Configuration.fromConfig()

  val allCommands: NonEmptyList[AnyCommand] = NonEmptyList.of(
    Help,
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
    allCommands.collect {
      case command: SlashCommand =>
        val data = command.pattern.build.setDefaultEnabled(true)
        IO.println(s"""Registering "${data.getName}", enabled? ${data.isDefaultEnabled}""") *> jda.upsertCommand(data).toIO
    }.sequence_


  override def run: IO[Unit] =
    (for
      jda <- Stream.eval(jdaIO)
      _ <- Stream.eval(registerSlashCommands(jda))
    yield ()).compile.drain