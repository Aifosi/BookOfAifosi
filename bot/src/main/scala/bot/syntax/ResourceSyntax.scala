package bot.syntax

import cats.effect.{IO, Resource}
import fs2.Stream

trait ResourceSyntax:
  extension[A] (resource: Resource[IO, A])
    def streamed: Stream[IO, A] = Stream.resource(resource)
