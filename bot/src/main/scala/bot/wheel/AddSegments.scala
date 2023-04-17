package bot.wheel

import bot.{Bot, DiscordLogger}
import bot.chaster.SegmentType.Text
import bot.chaster.{ChasterClient, Lock, Segment, SegmentType, WheelOfFortuneConfig}
import bot.commands.Compress
import bot.db.RegisteredUserRepository
import bot.db.Filters.*
import bot.model.{ChasterID, RegisteredUser}
import bot.syntax.io.*
import bot.tasks.TextWheelCommand
import bot.wheel.AddSegments.*
import cats.data.OptionT
import cats.effect.IO
import io.circe.parser.decode
import io.circe.syntax.*
import io.circe.{Decoder, Encoder}
import org.typelevel.log4cats.Logger

import java.io.{BufferedInputStream, ByteArrayInputStream, ByteArrayOutputStream}
import java.nio.{ByteBuffer, HeapByteBuffer}
import java.nio.charset.StandardCharsets
import java.util
import java.util.{Arrays, Base64}
import java.util.zip.{Deflater, GZIPInputStream, GZIPOutputStream, Inflater}
import scala.annotation.tailrec
import scala.concurrent.duration.*
import scala.util.Try
import scala.util.matching.Regex

class AddSegments(
  client: ChasterClient,
  registeredUserRepository: RegisteredUserRepository,
)(using discordLogger: DiscordLogger) extends TextWheelCommand(client, registeredUserRepository):
  override lazy val pattern: Regex = "AddSegments: (.+?)".r

  override def run(user: RegisteredUser, lock: Lock, text: String)(using Logger[IO]): IO[Boolean] =
    text match
      case pattern(text) =>
        authenticatedEndpoints(lock).semiflatMap { authenticatedEndpoints =>
          for
            _ <- authenticatedEndpoints.updateExtension[WheelOfFortuneConfig](lock._id) { configUpdate =>
              configUpdate.copy(
                config = configUpdate.config.copy(
                  segments = configUpdate.config.segments ++ text.decodeSegments
                ),
              )
            }
            _ <- Logger[IO].debug(s"Added $text segments to $user Wheel of fortune")
            _ <- discordLogger.logToSpinlog(s"$text segments to ${user.mention} Wheel of fortune")
          yield ()
        }
          .fold(false)(_ => true)
      case _ => IO.pure(false)

  override val description: String = s"Adds segments to the wheel, the segments text is compressed, use ${Compress.fullCommand} command to compress them"



object AddSegments:
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
          case (start, end) :: Nil => decoded.substring(start + 1, end).split(", ?").map(Segment(_)).toList
          case (start, end) :: (innerArrayStart, innerArrayEnd) :: _ =>
            val innerArray = decoded.substring(innerArrayStart, innerArrayEnd + 1)
            val replacedString = decoded.substring(start + 1, innerArrayStart) + "[]" + decoded.substring(innerArrayEnd + 1, end)
            replacedString.split(", ").toList.flatMap { segmentText =>
              innerArray.deflate.toOption.map{ innerArray =>
                Segment(segmentText.replace("[]", innerArray))
              }
            }
      }
