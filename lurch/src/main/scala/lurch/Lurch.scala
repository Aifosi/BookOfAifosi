package lurch

import bot.{ChannelConfig as _, *}
import bot.commands.*
import bot.model.Discord
import bot.tasks.Streams
import cats.data.NonEmptyList
import cats.effect.unsafe.IORuntime
import cats.effect.{Deferred, ExitCode, IO, IOApp}
import doobie.util.transactor.Transactor
import fs2.Stream
import lurch.commands.*
import lurch.tasks.{PilloryWinner, UpdateUsers, WheelTasks}
import net.dv8tion.jda.api.JDABuilder
import net.dv8tion.jda.api.requests.GatewayIntent
import org.flywaydb.core.Flyway
import org.http4s.blaze.client.BlazeClientBuilder
import org.http4s.blaze.server.BlazeServerBuilder
import org.http4s.{Request, Response, Status}
import org.http4s.client.Client
import org.http4s.client.middleware.Retry
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger

import java.time.LocalDateTime
import java.time.format.DateTimeFormatter
import java.time.temporal.ChronoUnit
import scala.concurrent.duration.FiniteDuration

//https://discord.com/oauth2/authorize?client_id=987840268726312970&scope=bot%20applications.commands&permissions=534992186432
//https://discord.com/oauth2/authorize?client_id=990221153203281950&scope=bot%20applications.commands&permissions=534992186432 - Test
//Add view channels permission
//TODO use streams to list from database
//TODO add unregister
object Lurch extends Bot:
  lazy val postgres: PostgresConfiguration = PostgresConfiguration.fromConfig()
  lazy val mysql: MysqlConfiguration = MysqlConfiguration.fromConfig()
  lazy val config: Configuration = Configuration.fromConfig()
  lazy val channels: ChannelConfig = ChannelConfig.fromConfig()

  lazy val commands: List[AnyCommand] = List(
    EnablePilloryBitches,
    DisablePilloryBitches,
    PilloryChecker,
    TriggerMessage,
    MessageDeleter,
    LockChannel,
    UnlockChannel,
    Task,
    TaskCompleter,
  )

  lazy val tasks: NonEmptyList[Streams] = NonEmptyList.of(
    PilloryWinner,
    UpdateUsers,
    WheelTasks,
  )
