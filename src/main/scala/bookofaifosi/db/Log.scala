package bookofaifosi.db

import bookofaifosi.Bot
import doobie.util.log.*
import bookofaifosi.syntax.logger.*
import cats.effect.unsafe.implicits.global

object Log:
  val handler: LogHandler = LogHandler {
    case Success(s, a, e1, e2) =>
      Bot.logger.debug(s"""Successful Statement Execution: ${s.linesIterator.dropWhile(_.trim.isEmpty).mkString("\n  ")}
                        | arguments = [${a.mkString(", ")}]
                        |   elapsed = ${e1.toMillis.toString} ms exec + ${e2.toMillis.toString} ms processing (${(e1 + e2).toMillis.toString} ms total)""".stripMargin).unsafeRunSync()

    case ProcessingFailure(s, a, e1, e2, t) =>
      Bot.logger.error(s"""Failed Resultset Processing: ${s.linesIterator.dropWhile(_.trim.isEmpty).mkString("\n  ")}
                          | arguments = [${a.mkString(", ")}]
                          |   elapsed = ${e1.toMillis.toString} ms exec + ${e2.toMillis.toString} ms processing (failed) (${(e1 + e2).toMillis.toString} ms total)
                          |   failure = ${t.getMessage}""".stripMargin).unsafeRunSync()

    case ExecFailure(s, a, e1, t) =>
      Bot.logger.error(s"""Failed Statement Execution: ${s.linesIterator.dropWhile(_.trim.isEmpty).mkString("\n  ")}
                          | arguments = [${a.mkString(", ")}]
                          |   elapsed = ${e1.toMillis.toString} ms exec (failed)
                          |   failure = ${t.getMessage}""".stripMargin).unsafeRunSync()
  }
