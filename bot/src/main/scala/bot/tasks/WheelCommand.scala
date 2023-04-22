package bot.tasks

import bot.{Bot, DiscordLogger}
import bot.chaster.ChasterClient
import bot.chaster.model.*
import bot.syntax.kleisli.*
import bot.db.Filters.*
import bot.db.RegisteredUserRepository
import bot.model.{ChasterID, DiscordID, RegisteredUser}
import bot.syntax.io.*
import bot.instances.functionk.given
import bot.tasks.ModifierTextWheelCommand.Modifier
import bot.tasks.ModifierTextWheelCommand.Modifier.*
import cats.data.{Kleisli, OptionT}
import cats.effect.IO
import cats.syntax.traverse.*
import org.typelevel.log4cats.Logger

import scala.util.matching.Regex
import scala.reflect.Typeable

abstract class WheelCommand[Pattern](chasterClient: ChasterClient, registeredUserRepository: RegisteredUserRepository)(using DiscordLogger):
  def pattern: Pattern
  def description: String

  def keyholder(lock: Lock, guildId: DiscordID): OptionT[IO, RegisteredUser] =
    for
      chasterKeyholder <- OptionT.fromOption(lock.keyholder)
      keyholder <- registeredUserRepository.find(chasterKeyholder._id.equalChasterID, guildId.equalGuildID)
    yield keyholder

  def apply(user: RegisteredUser, lock: Lock, segment: Segment)(using Logger[IO]): IO[(Boolean, Segment)]


abstract class TextWheelCommand(
  client: ChasterClient,
  registeredUserRepository: RegisteredUserRepository,
)(using DiscordLogger) extends WheelCommand[Regex](client, registeredUserRepository):
  override def apply(user: RegisteredUser, lock: Lock, segment: Segment)(using Logger[IO]): IO[(Boolean, Segment)] =
    if pattern.matches(segment.text) then run(user, lock, segment.text).map((_, segment)) else IO.pure((false, segment))

  def run(user: RegisteredUser, lock: Lock, text: String)(using Logger[IO]): IO[Boolean]

abstract class ModifierTextWheelCommand[Config <: ExtensionConfig: Typeable](
  client: ChasterClient,
  registeredUserRepository: RegisteredUserRepository
)(using discordLogger: DiscordLogger) extends TextWheelCommand(client, registeredUserRepository):
  def textPattern: String
  def logName: String

  def configUpdate(configUpdate: ConfigUpdate[Config], modifier: Modifier)(using Typeable[Config]): ConfigUpdate[Config]

  override lazy val pattern: Regex = s"$textPattern ${ModifierTextWheelCommand.modifierRegex}".r

  override def run(user: RegisteredUser, lock: Lock, text: String)(using Logger[IO]): IO[Boolean] =
    text match
      case pattern(modifierString, value) =>
        val maybeModifierString = Option(modifierString)
        IO.fromOption(value.toIntOption)(new Exception(s"Could not convert $value to Int")).flatMap { value =>
          val modifier = maybeModifierString match
            case Some("+") => Add(value)
            case Some("-") => Remove(value)
            case Some("*") => Multiply(value)
            case Some("/") => Divide(value)
            case _         => Exact(value)

          keyholder(lock, user.guildID).semiflatMap { keyholder =>
            for
              _ <- client.updateExtension[Config](lock._id)(configUpdate(_, modifier)).runUsingTokenOf(keyholder)
              message = maybeModifierString.fold("to ")(sign => s"by $sign") + value
              _ <- Logger[IO].debug(s"$user $logName changed $message")
              _ <- discordLogger.logToSpinlog(s"${user.mention} $logName changed $message")
            yield ()
          }
            .fold(false)(_ => true)
        }
      case _ => IO.pure(false)

object ModifierTextWheelCommand:
  val modifierRegex = "([-+*/])?(\\d+)".r

  enum Modifier(val apply: Int => Int):
    case Add(value: Int) extends Modifier(_ + value)
    case Remove(value: Int) extends Modifier(_ - value)
    case Exact(value: Int) extends Modifier(_ => value)
    case Multiply(value: Int) extends Modifier(_ * value)
    case Divide(value: Int) extends Modifier(_ / value)
