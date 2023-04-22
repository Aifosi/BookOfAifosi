package bot.chaster.model

import bot.chaster.model.*
import bot.chaster.model.instances.given

import cats.syntax.functor.*
import io.circe.{Decoder, Encoder}
import io.circe.syntax.*
import scala.concurrent.duration.FiniteDuration
import scala.util.Try

sealed trait ExtensionConfig

object ExtensionConfig:
  given Decoder[ExtensionConfig] = List[Decoder[ExtensionConfig]](
    Decoder[LinkConfig].widen,
    Decoder[PilloryConfig].widen,
    Decoder[DiceConfig].widen,
    Decoder[WheelOfFortuneConfig].widen,
    Decoder[TasksConfig].widen,
    Decoder[PenaltyConfig].widen,
    Decoder[TemporaryOpeningConfig].widen,
    Decoder[VerificationPictureConfig].widen,
    Decoder[RandomEventsConfig].widen,
    Decoder[GuessTimerConfig].widen,
    Decoder[PlayCardsConfig].widen,
  ).reduceLeft(_.or(_))

  given Encoder[ExtensionConfig] = Encoder.instance {
    case config: LinkConfig                => config.asJson
    case config: PilloryConfig             => config.asJson
    case config: DiceConfig                => config.asJson
    case config: WheelOfFortuneConfig      => config.asJson
    case config: TasksConfig               => config.asJson
    case config: PenaltyConfig             => config.asJson
    case config: TemporaryOpeningConfig    => config.asJson
    case config: VerificationPictureConfig => config.asJson
    case config: RandomEventsConfig        => config.asJson
    case config: GuessTimerConfig          => config.asJson
    case config: PlayCardsConfig           => config.asJson
  }

case class LinkConfig(
  timeToAdd: FiniteDuration,
  timeToRemove: FiniteDuration,
  enableRandom: Boolean,
  nbVisits: Int,
  limitToLoggedUsers: Boolean,
) extends ExtensionConfig
    derives Decoder, Encoder.AsObject

case class PilloryConfig(
  timeToAdd: FiniteDuration,
  limitToLoggedUsers: Boolean,
) extends ExtensionConfig
    derives Decoder, Encoder.AsObject

case class DiceConfig(
  multiplier: FiniteDuration,
) extends ExtensionConfig
    derives Decoder, Encoder.AsObject

case class WheelOfFortuneConfig(
  segments: List[Segment],
) extends ExtensionConfig
    derives Decoder, Encoder.AsObject

case class Task(
  task: String,
  points: Int,
) derives Decoder, Encoder.AsObject

case class TasksConfig(
  tasks: List[Task],
  voteEnabled: Boolean,
  voteDuration: FiniteDuration,
  startVoteAfterLastVote: Boolean,
  enablePoints: Boolean,
  pointsRequired: Int,
  allowWearerToEditTasks: Boolean,
  allowWearerToConfigureTasks: Boolean,
  preventWearerFromAssigningTasks: Boolean,
  allowWearerToChooseTasks: Boolean,
  actionsOnAbandonedTask: List[Punishment],
) extends ExtensionConfig
    derives Decoder, Encoder.AsObject

case class PenaltyConfig(
  penalties: List[PunishmentConfig],
) extends ExtensionConfig
    derives Decoder, Encoder.AsObject

case class TemporaryOpeningConfig(
  openingTime: FiniteDuration,
  penaltyTime: FiniteDuration,
  allowOnlyKeyholderToOpen: Boolean,
) extends ExtensionConfig
    derives Decoder, Encoder.AsObject

case class PeerVerification(
  enabled: Boolean,
  punishments: List[Punishment],
) derives Decoder, Encoder.AsObject

case class VerificationPictureConfig(
  visibility: String,
  peerVerification: PeerVerification,
) extends ExtensionConfig
    derives Decoder, Encoder.AsObject

enum RandomEventsDifficulty:
  case Easy, Normal, Hard, Expert

object RandomEventsDifficulty:
  given Decoder[RandomEventsDifficulty] = Decoder[String].emapTry { string =>
    Try(RandomEventsDifficulty.valueOf(string.capitalize))
  }
  given Encoder[RandomEventsDifficulty] = Encoder[String].contramap(_.toString.toLowerCase)

case class RandomEventsConfig(
  difficulty: RandomEventsDifficulty,
) extends ExtensionConfig
    derives Decoder, Encoder.AsObject

case class GuessTimerConfig(
  minRandomTime: FiniteDuration,
  maxRandomTime: FiniteDuration,
) extends ExtensionConfig
    derives Decoder, Encoder.AsObject

enum CardColour:
  case Red, Green, Yellow, Purple, Freeze, Reset, Sticky

object CardColour:
  given Decoder[CardColour] = Decoder[String].emapTry { string =>
    Try(CardColour.valueOf(string.capitalize))
  }
  given Encoder[CardColour] = Encoder[String].contramap(_.toString.toLowerCase)

case class Card(
  `type`: CardColour,
  min: Int,
  max: Int,
) derives Decoder, Encoder.AsObject

case class PlayCardsConfig(
  regularity: FiniteDuration,
  mode: AvailableModes,
  nbKeysRequired: Int,
  cards: List[Card],
) extends ExtensionConfig
    derives Decoder, Encoder.AsObject

object PlayCardsConfig:
  def apply(regularity: FiniteDuration, nbKeysRequired: Int, cards: Card*): PlayCardsConfig =
    new PlayCardsConfig(regularity, AvailableModes.Unlimited, nbKeysRequired, cards.toList)

//StayLockedTheWholeMonthConfig has no config aparently
/*case class StayLockedTheWholeMonthConfig(
) extends ExtensionConfig*/
