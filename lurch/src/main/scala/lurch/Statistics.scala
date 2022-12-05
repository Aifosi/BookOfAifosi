package lurch

import bot.db.RegisteredUserRepository
import bot.model.{ChasterID, DiscordID, RegisteredUser}
import fs2.Stream
import bot.syntax.all.*
import bot.db.Filters.*
import bot.chaster.Client.*
import bot.chaster.Client.given
import bot.chaster.{DiceRolledPayload, Event, SegmentType, TimeChangedPayload, WheelTurnedPayload}
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

object Statistics:
  extension (number: Double)
    def round(precision: Int): Double =
      val d = math.pow(10, precision)
      math.round(number * d) / d

  given Show[FiniteDuration] = (duration: FiniteDuration) =>
    val zero = 0.seconds
    var remaining = duration
    var text = ""
    while remaining != zero do
      lazy val days = remaining.toHours / 24
      lazy val hours = remaining.toMinutes / 60
      lazy val minutes = remaining.toSeconds / 60
      if days != 0 then
        text += s"$days d"
        remaining -= days.days
      else if hours != 0 then
        text += s"${if text.isEmpty then "" else " "}$hours h"
        remaining -= hours.hours
      else if minutes != 0 then
        text += s"${if text.isEmpty then "" else " "}$minutes m"
        remaining -= minutes.minutes
      else
        text += s"${if text.isEmpty then "" else " "}${remaining.toSeconds} s"
        remaining = zero
    text

  given Show[(String, (Int, Int))] = {
    case (user, (add, remove)) => s"$user (A: $add, R: $remove)"

  }
  given Show[Event[?]] = (event: Event[?]) =>
    s"Event(extension: ${event.extension}, id: ${event._id}, type: ${event.`type`} createdAt: ${event.createdAt})"

  val discordIDS = List(
    ("Taco's Lock", DiscordID(871413910551531550), ChasterID("6377dace0dc91be5a3f82540")), //My ID, Janice's lock
  )

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
    commands: Map[String, Int] = Map.empty,
    guessTimer: Int = 0,
    guessTimerAdded: FiniteDuration = 0.seconds,
  ):
    override def toString: String =
      val maxVotes = votes.map {
        case (user, (add, remove)) => add + remove
      }.max
      val maxAdds = votes.map {
        case (user, (add, remove)) => add
      }.max
      val maxRemoves = votes.map {
        case (user, (add, remove)) => remove
      }.max
      val mostVotes = votes.filter {
        case (user, (add, remove)) => (add + remove) == maxVotes
      }.map(a => show"$a").mkString(", ")
      val mostAdds = votes.filter {
        case (user, (add, remove)) => add == maxAdds
      }.map(a => show"$a").mkString(", ")
      val mostRemoves = votes.filter {
        case (user, (add, remove)) => remove == maxRemoves
      }.map(a => show"$a").mkString(", ")
      val totalVotes = votes.map {
        case (user, (add, remove)) => (user, add + remove)
      }
        .groupBy(_._2)
        .view
        .mapValues(_.size)
        .toList
        .sortBy(_._1)(Ordering[Int].reverse)
        .map {
          case (1, totalUsers) => s"Voters that voted once: $totalUsers"
          case (2, totalUsers) => s"Voters that voted twice: $totalUsers"
          case (number, totalUsers) => s"Voters that voted $number times: $totalUsers"
        }


      show"""Total time added: $timeAdded
            |Events: $events
            |Dice - Rolls: $diceRolls, Time added: $diceTimeAdded
            |Wheel - Rolls: $wheelRolls, Time added: $wheelTimeAdded
            |Commands:
            |${commands.mkString("\n")}
            |Votes - Adds: $voteAdds, Removes: $voteRemoves, Net adds: ${voteAdds - voteRemoves}
            |Unique Voters: ${votes.size}
            |Top
            |  Votes: $mostVotes
            |  Adds: $mostAdds
            |  Removes: $mostRemoves
            |${totalVotes.mkString("\n")}
            |""".stripMargin

    lazy val addDiceRoll: Results =
      copy(
        events = events + 1,
        diceRolls = diceRolls + 1,
      )
    def addDiceRollTime(time: FiniteDuration): Results =
      copy(
        timeAdded = timeAdded + time,
        diceTimeAdded = diceTimeAdded + time,
      )
    lazy val addWheelRoll: Results =
      copy(
        events = events + 1,
        wheelRolls = wheelRolls + 1,
      )
    def addWheelRollTime(time: FiniteDuration): Results =
      copy(
        timeAdded = timeAdded + time,
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
    def addCommand(command: String): Results =
      copy(
        events = events + 1,
        wheelRolls = wheelRolls + 1,
        commands = commands.updatedWith(command)(_.fold(1)(_ + 1).some),
      )
    def addGuessTimer(time: FiniteDuration): Results =
      copy(
        guessTimer = guessTimer + 1,
        timeAdded = timeAdded + time,
        diceTimeAdded = diceTimeAdded + time,
      )

    def addEvent(event: Event[Json]): Results =
    /*val lockStart = LocalDateTime.of(2022, 10, 19, 21, 36).toInstant(ZoneOffset.UTC)
    val lockEnd = lockStart.plusSeconds(600)
    if event.createdAt.isAfter(lockStart) && event.createdAt.isBefore(lockEnd) then println(show"$event")*/

      event.`type` match
        case "link_time_changed" =>
          (for
            user <- event.user
            event <- event.as[TimeChangedPayload]
            results = if event.payload.duration > 0.seconds then addVoteAdd(user.username) else addVoteRemove(user.username)
          yield results).getOrElse(throw new Exception("Vote without user"))

        case "time_changed" if event.role == "keyholder" => this
        case "time_changed" =>
          (for
            extension <- event.extension
            event <- event.as[TimeChangedPayload]
            results = extension match {
              case "wheel-of-fortune" => addWheelRollTime(event.payload.duration)
              case "dice" => addDiceRollTime(event.payload.duration)
              case "guess-timer" => addGuessTimer(event.payload.duration)
              case _ =>
                println(event)
                this
            }
          yield results).getOrElse(throw new Exception(s"Can't parse time changed event $event"))
        case "dice_rolled" => addDiceRoll
        case "wheel_of_fortune_turned" =>
          val wheelOfFortune = event.as[WheelTurnedPayload].getOrElse(throw new Exception("Can't parse wheel of fortune turned event"))
          wheelOfFortune.payload.segment match
            case segment if segment.`type` == SegmentType.Text => addCommand(segment.text)
            case _ => addWheelRoll
        case _ => this


  val run: Stream[IO, Unit] =
    for
      given Logger[IO] <- Slf4jLogger.create[IO].streamed
      (name, discordID, lockID) <- Stream.emits(discordIDS)
      user <- RegisteredUserRepository.get(discordID.equalDiscordID).streamed
      _ <- IO.println(s"Parsing history for $name").streamed
      events <- user.lockHistory(lockID)
        .compile
        .toList
        .streamed
      results: Results = events.foldLeft(Results())(_.addEvent(_))
      _ <- IO.println(results.toString).streamed
    yield ()
