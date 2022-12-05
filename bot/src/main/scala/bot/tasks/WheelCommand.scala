package bot.tasks

import bot.Bot
import bot.chaster.{ConfigUpdate, DiceConfig, ExtensionConfig, Lock, Segment, SegmentType}
import bot.chaster.Client.{*, given}
import bot.db.Filters.*
import bot.db.RegisteredUserRepository
import bot.model.{ChasterID, RegisteredUser}
import bot.syntax.io.*
import bot.tasks.ModifierTextWheelCommand.Modifier
import bot.tasks.ModifierTextWheelCommand.Modifier.*
import cats.data.OptionT
import cats.effect.IO
import cats.syntax.traverse.*
import org.typelevel.log4cats.Logger

import scala.util.matching.Regex
import scala.reflect.Typeable

abstract class WheelCommand:
  def apply(user: RegisteredUser, lock: Lock, segment: Segment)(using Logger[IO]): IO[(Boolean, Segment)]

trait SimpleWheelCommand[T]:
  this: WheelCommand =>
  def modifier(segment: Segment): T
  def run(user: RegisteredUser, lock: Lock, t: T)(using Logger[IO]): IO[Boolean]

  final override def apply(user: RegisteredUser, lock: Lock, segment: Segment)(using Logger[IO]): IO[(Boolean, Segment)] =
    run(user, lock, modifier(segment)).map((_, segment))


abstract class TextWheelCommand extends WheelCommand:
  def pattern: Regex

  override def apply(user: RegisteredUser, lock: Lock, segment: Segment)(using Logger[IO]): IO[(Boolean, Segment)] =
    if pattern.matches(segment.text) then run(user, lock, segment.text).map((_, segment)) else IO.pure((false, segment))

  def run(user: RegisteredUser, lock: Lock, text: String)(using Logger[IO]): IO[Boolean]

def keyholder(lock: Lock)(using Logger[IO]): OptionT[IO, RegisteredUser] =
  for
    chasterKeyholder <- OptionT.fromOption(lock.keyholder)
    keyholder <- OptionT(RegisteredUserRepository.find(chasterKeyholder._id.equalChasterID))
  yield keyholder

abstract class ModifierTextWheelCommand[Config <: ExtensionConfig: Typeable] extends TextWheelCommand:
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

          (for
            keyholder <- keyholder(lock)
            _ <- OptionT.liftF(keyholder.updateExtension[Config](lock._id)(configUpdate(_, modifier)))
            message = maybeModifierString.fold("to ")(sign => s"by $sign") + value
            _ <- OptionT.liftF(Logger[IO].debug(s"$user $logName changed $message"))
            _ <- Bot.channels.spinlog.sendMessage(s"${user.mention} $logName changed $message")
          yield ())
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
