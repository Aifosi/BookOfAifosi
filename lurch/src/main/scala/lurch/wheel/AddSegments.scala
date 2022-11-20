package lurch.wheel

import bot.chaster.{Segment, SegmentType, WheelOfFortuneConfig}
import bot.chaster.Client.{*, given}
import bot.chaster.SegmentType.Text
import bot.model.{ChasterID, RegisteredUser}
import bot.syntax.io.*
import bot.tasks.TextWheelCommand
import cats.data.OptionT
import cats.effect.IO
import io.circe.parser.decode
import io.circe.syntax.*
import io.circe.{Decoder, Encoder}
import lurch.Lurch
import org.typelevel.log4cats.Logger

import java.nio.ByteBuffer
import scala.util.matching.Regex
import java.util.Base64
import java.nio.charset.StandardCharsets
import java.nio.HeapByteBuffer
import scala.annotation.tailrec
import scala.concurrent.duration.*
import java.io.{BufferedInputStream, ByteArrayInputStream, ByteArrayOutputStream}
import java.util
import java.util.Arrays
import java.util.zip.{GZIPInputStream, GZIPOutputStream}
import java.util.zip.{Inflater, Deflater}
import scala.util.Try

object AddSegments extends TextWheelCommand {
  override lazy val pattern: Regex = "AddSegments: (.+?)".r

  extension (string: String)
    def inflate: Try[String] = Try {
      val bytes = Base64.getDecoder.decode(string)
      val zipInputStream = GZIPInputStream(new ByteArrayInputStream(bytes))
      new String(zipInputStream.readAllBytes(), StandardCharsets.UTF_8)
    }

    def deflate: Try[String] = Try {
      val arrOutputStream = new ByteArrayOutputStream()
      val zipOutputStream = new GZIPOutputStream(arrOutputStream)
      zipOutputStream.write(string.getBytes)
      zipOutputStream.close()
      Base64.getEncoder.encodeToString(arrOutputStream.toByteArray)
    }

    def indexesOf(char: String): List[Int] =
      @tailrec def inner(string: String, startingIndex: Int = 0, indexes: List[Int] = List.empty): List[Int] =
        string.indexOf(char, startingIndex) match
          case -1 => indexes
          case i => inner(string, i + 1, indexes :+ i)
      inner(string)

    def decodeSegments: List[Segment] =
      string.inflate.toOption.toList.flatMap { decoded =>
        val starts = decoded.indexesOf("[")
        val ends = decoded.indexesOf("]").reverse
        starts.zip(ends) match
          case Nil => Nil
          case (start, end) :: Nil => decoded.substring(start + 1, end).split(", ").map(Segment(_)).toList
          case (start, end) :: (innerArrayStart, innerArrayEnd) :: _ =>
            val innerArray = decoded.substring(innerArrayStart, innerArrayEnd + 1)
            val replacedString = decoded.substring(start + 1, innerArrayStart) + "[]" + decoded.substring(innerArrayEnd + 1, end)
            replacedString.split(", ").toList.flatMap { segmentText =>
              innerArray.deflate.toOption.map{ innerArray =>
                Segment(segmentText.replace("[]", innerArray))
              }
            }
      }

  override def run(user: RegisteredUser, lockID: ChasterID, text: String)(using Logger[IO]): IO[Boolean] =
    text match {
      case pattern(text) =>
        lazy val segments = text.decodeSegments
        (for
          (_, keyholder) <- lockAndKeyholder(user, lockID)
          _ <- OptionT.liftF {
            keyholder.updateExtension[WheelOfFortuneConfig](lockID) { configUpdate =>
              configUpdate.copy(
                config = configUpdate.config.copy(
                  segments = configUpdate.config.segments ++ segments
                ),
              )
            }
          }
          _ <- OptionT.liftF(Logger[IO].debug(s"Added $text segments to $user Wheel of fortune"))
          _ <- Lurch.channels.spinlog.sendMessage(s"$text segments to ${user.mention} Wheel of fortune")
        yield ())
          .fold(false)(_ => true)
      case _ => IO.pure(false)
    }


}
