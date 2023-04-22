package bot.commands

import bot.chaster.ChasterClient
import bot.db.RegisteredUserRepository
import bot.instances.functionk.given
import bot.model.UserToken
import bot.model.event.{MessageEvent, ReactionEvent}
import bot.syntax.kleisli.*

import cats.data.{EitherT, Kleisli}
import cats.effect.IO
import org.typelevel.log4cats.Logger
import scala.util.matching.Regex

object PilloryVoter extends TextCommand with Hidden with NoLog:
  val add = "⏫"

  override val pattern: Regex = ".*http(?:s)?://chaster.app/activity/(\\w{24}).*".r

  override def apply(pattern: Regex, event: MessageEvent)(using Logger[IO]): IO[Boolean] =
    for _ <- event.message.addReaction(add)
    yield true

  def vote(
    client: ChasterClient,
    registeredUserRepository: RegisteredUserRepository,
    event: ReactionEvent,
  ): EitherT[IO, String, Unit] =
    Voter.getRegisteredUserAndChasterID(pattern, registeredUserRepository, event).semiflatMap { (user, pilloryId) =>
      Kleisli { (token: UserToken) =>
        for
          post      <- client.post(pilloryId)
          voteResult = client.voteOnPillory(post.lock._id, post.extensionParty._id, post.data._id).run(token)
          _         <- voteResult.foldF(
                         user.sendMessage,
                         _ => user.sendMessage(s"I've voted on ${post.user.username}'s pillory on your behalf!"),
                       )
        yield ()
      }
        .runUsingTokenOf(user)
    }
