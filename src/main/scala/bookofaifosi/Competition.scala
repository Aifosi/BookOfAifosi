package bookofaifosi

import bookofaifosi.db.RegisteredUserRepository
import bookofaifosi.model.{ChasterID, DiscordID, RegisteredUser}
import fs2.Stream
import bookofaifosi.syntax.all.*
import bookofaifosi.db.Filters.*
import bookofaifosi.chaster.Client.*
import bookofaifosi.chaster.Client.given
import bookofaifosi.chaster.{DiceRolledPayload, Event, WheelTurnedPayload}
import cats.Show
import cats.data.OptionT
import cats.effect.IO
import cats.syntax.option.*
import cats.syntax.show.*
import io.circe.Json
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger

import java.time.{Instant, LocalDateTime, ZoneOffset}
import scala.concurrent.duration.*

object Competition:
  extension (number: Double)
    def round(precision: Int): Double =
      val d = math.pow(10, precision)
      math.round(number * d) / d

  given Show[FiniteDuration] = (duration: FiniteDuration) =>
    if duration > 1.day || duration < -1.day then s"${(duration.toHours / 24D).round(1)}d" else s"${(duration.toMinutes / 60D).round(1)}h"

  given Show[(String, (Int, Int))] = {
    case (user, (add, remove)) => s"$user (A: $add, R: $remove)"
  }

  val discordIDS = List((DiscordID(875406477647548447), ChasterID("634dcc94f944271920b43518")), (DiscordID(871413910551531550), ChasterID("629aa50ffd8ea767923df229")))
  val illegalSpins = List("63506e2e4f4e92a651940444", "63506ee8248794a57d56979d", "63506f01d0d3e8bf1907d47f", "63506f3b74bc2803f246eef6").map(ChasterID(_))

  case class Results(
    events: Int = 0,
    timeAdded: FiniteDuration = 0.seconds,
    diceRolls: Int = 0,
    diceTimeAdded: FiniteDuration = 0.seconds,
    wheelRolls: Int = 0,
    wheelTimeAdded: FiniteDuration = 0.seconds,
    votesTimeAdded: FiniteDuration = 0.seconds,
    voteRemoves: Int = 0,
    voteAdds: Int = 0,
    votes: Map[String, (Int, Int)] = Map.empty,
  ):
    override def toString: String =
      val mostVotes = votes.maxBy {
        case (user, (add, remove)) => add + remove
      }
      val mostAdds = votes.maxBy {
        case (user, (add, remove)) => add
      }
      val mostRemoves = votes.maxBy {
        case (user, (add, remove)) => remove
      }

      show"""Total time added: $timeAdded (${timeAdded.toSeconds} s)
        |Events: $events
        |Dice - Rolls: $diceRolls, Time added: $diceTimeAdded
        |Wheel - Rolls: $wheelRolls, Time added: $wheelTimeAdded
        |Votes - Adds: $voteAdds, Removes: $voteRemoves
        |Top: Votes: $mostVotes, Adds: $mostAdds, Removes: $mostRemoves
        |Unique Voters: ${votes.size}
        |""".stripMargin

    def addDiceRoll(adminRoll: Int, userRoll: Int): Results =
      val time = (userRoll - adminRoll).days
      copy(
        events = events + 1,
        timeAdded = timeAdded + time,
        diceRolls = diceRolls + 1,
        diceTimeAdded = diceTimeAdded + time,
      )
    def addWheelRoll(time: FiniteDuration): Results =
      copy(
        events = events + 1,
        timeAdded = timeAdded + time,
        wheelRolls = wheelRolls + 1,
        wheelTimeAdded = wheelTimeAdded + time,
      )
    def addVoteAdd(user: String): Results =
      copy(
        events = events + 1,
        timeAdded = timeAdded + 3.hours,
        votes = votes.updatedWith(user)(_.fold((1, 0))((adds, removes) => (adds + 1, removes)).some),
        votesTimeAdded = votesTimeAdded + 3.hours,
        voteAdds = voteAdds + 1
      )
    def addVoteRemove(user: String): Results =
      copy(
        events = events + 1,
        timeAdded = timeAdded - 3.hours,
        votes = votes.updatedWith(user)(_.fold((0, 1))((adds, removes) => (adds, removes + 1)).some),
        votesTimeAdded = votesTimeAdded - 3.hours,
        voteRemoves = voteRemoves + 1
      )

    def addEvent(event: Event[Json]): Results =
      event.`type` match
        case "dice_rolled" =>
          event.as[DiceRolledPayload].fold(throw new Exception("Can't parse dice roll event")) { event =>
            addDiceRoll(event.payload.adminDice, event.payload.playerDice)
          }
        case "wheel_of_fortune_turned" =>
          event.as[WheelTurnedPayload].fold(throw new Exception("Cant parse wheel turned event")) {
            case event if event.payload.segment.`type`.equalsIgnoreCase("add-time") => addWheelRoll(event.payload.segment.duration.seconds)
            case event => addWheelRoll(-event.payload.segment.duration.seconds)
          }
        case "link_time_changed" =>
          event.user.fold(throw new Exception("Vote without user")) {
            case user if event.description.equalsIgnoreCase("Added time") => addVoteAdd(user.username)
            case user => addVoteRemove(user.username)
          }
        case _ => this


  val run: Stream[IO, Unit] =
    for
      given Logger[IO] <- Slf4jLogger.create[IO].streamed
      lockStart = LocalDateTime.of(2022, 10, 17, 21, 43).toInstant(ZoneOffset.UTC)
      lockEnd = lockStart.plusSeconds((3.days + 22.hours + 45.minutes).toSeconds)
      _ <- IO.println(s"lockStart: $lockStart").streamed
      _ <- IO.println(s"lockEnd: $lockEnd").streamed
      (discordID, lockID) <- Stream.emits(discordIDS)
      user <- RegisteredUserRepository.get(discordID.equalDiscordID).streamed
      _ <- IO.println(s"Parsing history for $user").streamed
      events <- user.lockHistory(lockID, lockStart.some)
        .filter(_.createdAt.isBefore(lockEnd))
        .filter(event => !illegalSpins.contains(event._id))
        .compile
        .toList
        .streamed
      results: Results = events.foldLeft(Results())(_.addEvent(_))
      _ <- IO.println(results.toString).streamed
    yield ()
