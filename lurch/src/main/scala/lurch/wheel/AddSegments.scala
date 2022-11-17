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

object AddSegments extends TextWheelCommand {
  override lazy val pattern: Regex = "AddSegments: (.+?)".r

  /*private val addTimePattern: Regex = "AddTime: (\\d+)".r
  private val removeTimePattern: Regex = "RemoveTime: (\\d+)".r
  private val addRemoveTimePattern: Regex = "AddRemoveTime: (\\d+)".r*/

  extension (string: String)
    def compress: String =
      val deflater = new Deflater()
      deflater.setInput(string.getBytes(StandardCharsets.UTF_8))
      deflater.finish()
      val compressedData = new Array[Byte](5000)
      val count = deflater.deflate(compressedData)
      deflater.end()
      new String(compressedData.take(count), StandardCharsets.UTF_8)

    def decompress: String =
      val array = new Array[Byte](5000)
      val inflater = new Inflater()
      inflater.setInput(string.getBytes(StandardCharsets.UTF_8))
      var count = inflater.inflate(array)
      var result = array.take(count)
      while count > 0 do
        count = inflater.inflate(array)
        result = result ++ array.take(count)
      inflater.end()
      new String(result, StandardCharsets.UTF_8)

    def decodeB64: String =
      StandardCharsets.UTF_8.decode(ByteBuffer.wrap(Base64.getDecoder.decode(string))).toString

    def encodeB64: String = Base64.getEncoder.encodeToString(string.getBytes(StandardCharsets.UTF_8))

    def indexesOf(char: String): List[Int] =
      @tailrec def inner(string: String, startingIndex: Int = 0, indexes: List[Int] = List.empty): List[Int] =
        string.indexOf(char, startingIndex) match
          case -1 => indexes
          case i => inner(string, i + 1, indexes :+ i)
      inner(string)

    def decodeSegments: List[Segment] =
      val decoded = string.decodeB64
      val starts = decoded.indexesOf("[")
      val ends = decoded.indexesOf("]").reverse
      println(starts)
      println(ends)
      starts.zip(ends) match
        case Nil => Nil
        case (start, end) :: Nil => decoded.substring(start + 1, end).split(", ").map(Segment(_)).toList
        case (start, end) :: (innerArrayStart, innerArrayEnd) :: _ =>
          val innerArray = decoded.substring(innerArrayStart, innerArrayEnd + 1)
          val replacedString = decoded.substring(start + 1, innerArrayStart) + "[]" + decoded.substring(innerArrayEnd + 1, end)
          replacedString.split(", ").map(segmentText => Segment(segmentText.replace("[]", innerArray.encodeB64))).toList

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
          _ <- Lurch.channels.spinlog.sendMessage(s"${user.mention} $text segments to $user Wheel of fortune")
        yield ())
          .fold(false)(_ => true)
      case _ => IO.pure(false)
    }


}
