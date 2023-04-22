package bot.syntax

import bot.Bot

import cats.effect.IO
import net.dv8tion.jda.api.requests.RestAction
import scala.annotation.targetName
import scala.concurrent.Promise

trait ActionSyntax:
  extension [A](action: RestAction[A])
    def toIO: IO[A] = IO.fromFuture(IO {
      val p = Promise[A]()
      action.queue(p.success, p.failure)
      p.future
    })
