package bookofaifosi.syntax

import cats.effect.IO
import net.dv8tion.jda.api.utils.concurrent.Task

import scala.annotation.targetName
import scala.concurrent.Promise

trait TaskSyntax:
  extension [A](task: Task[A])
    def toIO: IO[A] = IO.fromFuture(IO {
      val p = Promise[A]()
      task
        .onSuccess(p.success)
        .onError(p.failure)
      p.future
    })