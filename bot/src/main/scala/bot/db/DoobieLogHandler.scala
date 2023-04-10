package bot.db

import doobie.util.log.*
import cats.effect.IO
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger
import cats.effect.unsafe.IORuntime

object DoobieLogHandler:
  private def createLogHandler(using Logger[IO], IORuntime): LogHandler = LogHandler {
    case Success(s, a, e1, e2) =>
      Logger[IO].debug(s"""Successful Statement Execution: ${s.linesIterator.dropWhile(_.trim.isEmpty).mkString("\n  ")}
                        | arguments = [${a.mkString(", ")}]
                        |   elapsed = ${e1.toMillis.toString} ms exec + ${e2.toMillis.toString} ms processing (${(e1 + e2).toMillis.toString} ms total)""".stripMargin).unsafeRunSync()

    case ProcessingFailure(s, a, e1, e2, t) =>
      Logger[IO].error(s"""Failed Resultset Processing: ${s.linesIterator.dropWhile(_.trim.isEmpty).mkString("\n  ")}
                          | arguments = [${a.mkString(", ")}]
                          |   elapsed = ${e1.toMillis.toString} ms exec + ${e2.toMillis.toString} ms processing (failed) (${(e1 + e2).toMillis.toString} ms total)
                          |   failure = ${t.getMessage}""".stripMargin).unsafeRunSync()

    case ExecFailure(s, a, e1, t) =>
      Logger[IO].error(s"""Failed Statement Execution: ${s.linesIterator.dropWhile(_.trim.isEmpty).mkString("\n  ")}
                          | arguments = [${a.mkString(", ")}]
                          |   elapsed = ${e1.toMillis.toString} ms exec (failed)
                          |   failure = ${t.getMessage}""".stripMargin).unsafeRunSync()
  }
  
  def default(using IORuntime): IO[LogHandler] = 
    for
      given Logger[IO] <- Slf4jLogger.create[IO]
    yield createLogHandler
