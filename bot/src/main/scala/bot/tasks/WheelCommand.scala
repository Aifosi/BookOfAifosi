package bot.tasks

import bot.chaster.{Segment, SegmentType}
import bot.model.{ChasterID, RegisteredUser}
import bot.tasks.ModifierTextWheelCommand.Modifier
import bot.tasks.ModifierTextWheelCommand.Modifier.*
import cats.effect.IO
import org.typelevel.log4cats.Logger

import scala.util.matching.Regex

abstract class WheelCommand:
  def apply(user: RegisteredUser, lockID: ChasterID, segment: Segment)(using Logger[IO]): IO[(Boolean, Segment)]

trait SimpleWheelCommand[T]:
  this: WheelCommand =>
  def modifier(segment: Segment): T
  def run(user: RegisteredUser, lockID: ChasterID, t: T)(using Logger[IO]): IO[Boolean]

  final override def apply(user: RegisteredUser, lockID: ChasterID, segment: Segment)(using Logger[IO]): IO[(Boolean, Segment)] =
    run(user, lockID, modifier(segment)).map((_, segment))


abstract class TextWheelCommand extends WheelCommand:
  def pattern: Regex

  override def apply(user: RegisteredUser, lockID: ChasterID, segment: Segment)(using Logger[IO]): IO[(Boolean, Segment)] =
    if pattern.matches(segment.text) then run(user, lockID, segment.text).map((_, segment)) else IO.pure((false, segment))

  def run(user: RegisteredUser, lockID: ChasterID, text: String)(using Logger[IO]): IO[Boolean]

abstract class ModifierTextWheelCommand extends TextWheelCommand:
  def textPattern: String

  def run(user: RegisteredUser, lockID: ChasterID, modifier: Modifier)(using Logger[IO]): IO[Boolean]

  override lazy val pattern: Regex = s"$textPattern ${ModifierTextWheelCommand.modifierRegex}".r

  override def run(user: RegisteredUser, lockID: ChasterID, text: String)(using Logger[IO]): IO[Boolean] =
    text match
      case pattern(modifierString, value) =>
        IO.fromOption(value.toIntOption)(new Exception(s"Could not convert $value to Int")).flatMap { value =>
          val modifier = Option(modifierString) match
            case Some("+") => Add(value)
            case Some("-") => Remove(value)
            case _         => Exact(value)
          run(user, lockID, modifier)
        }
      case _ => IO.pure(false)

object ModifierTextWheelCommand:
  val modifierRegex = "(-|\\+)?(\\d+)".r

  enum Modifier(val apply: Int => Int):
    case Add(value: Int) extends Modifier(_ + value)
    case Remove(value: Int) extends Modifier(_ - value)
    case Exact(value: Int) extends Modifier(_ => value)
    
