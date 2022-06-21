package bookofaifosi.commands

import bookofaifosi.model.event.{SlashAPI, SlashCommandEvent}
import cats.effect.{IO, Ref}
import scala.concurrent.duration.*

trait SlowResponse:
  this: SlashCommand =>
  val ephemeralResponses: Boolean
  def slowResponse(pattern: SlashPattern, event: SlashCommandEvent, slashAPI: Ref[IO, SlashAPI]): IO[Unit]

  override final def apply(pattern: SlashPattern, event: SlashCommandEvent): IO[Boolean] =
    def switchToHook(slashAPI: Ref[IO, SlashAPI], repliedRef: Ref[IO, Boolean]) =
      for
        _ <- IO.sleep(3.seconds)
        replied <- repliedRef.get
        _ <- if !replied then
          slashAPI.set(event.hook) *> event.deferReply(ephemeralResponses) *> repliedRef.set(true)
        else
          IO.unit
      yield ()

    for
      slashAPI <- Ref.of[IO, SlashAPI](event)
      repliedRef <- Ref.of[IO, Boolean](false)
      _ <- switchToHook(slashAPI, repliedRef).start
      _ <- slowResponse(pattern, event, slashAPI)
      _ <- repliedRef.set(true)
    yield true
