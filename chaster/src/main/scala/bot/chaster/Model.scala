package bot.chaster

import cats.syntax.functor.*
import bot.model.ChasterID
import io.circe.{Decoder, Encoder, HCursor, Json}
import io.circe.syntax.*

import java.time.Instant
import scala.concurrent.duration.*
import scala.util.Try
import scala.util.chaining.*
import Millis.{Millis, given}

extension (string: String)
  private def charSeparatedToPascal(char: String): String =
    string.split(char).map(_.capitalize).mkString
  private def pascalToCharSeparated(char: String): String =
    string.replaceAll("(?<=[a-z])(?=[A-Z])", char).toLowerCase

  def snakeToPascalCase: String = charSeparatedToPascal("_")
  def pascalToSnakeCase: String = pascalToCharSeparated("_")

  def kebabToPascalCase: String = charSeparatedToPascal("-")
  def pascalToKebabCase: String = pascalToCharSeparated("-")

given Encoder[FiniteDuration] = Encoder[Long].contramap(_.toSeconds)
given Decoder[FiniteDuration] = Decoder[Long].map(_.seconds)

object Millis:
  opaque type Millis = FiniteDuration
  given Conversion[Millis, FiniteDuration] = identity
  given Encoder[Millis] = Encoder[Long].contramap(_.toMillis)
  given Decoder[Millis] = Decoder[Long].map(_.millis)

case class AccessToken(
  access_token: String,
  expires_in: Int,
  refresh_expires_in: Int,
  refresh_token: String,
  token_type: String,
  scope: String,
) derives Decoder:
  val expiresAt: Instant = Instant.now().plusSeconds(expires_in)

trait WithID:
  def _id: ChasterID

case class User(
  _id: ChasterID,
  username: String,
  gender: Option[String],
  role: String,
  discordId: Option[String],
  discordUsername: Option[String],
) extends WithID derives Decoder

case class PublicUser(
  _id: ChasterID,
  username: String,
  gender: Option[String],
  isDisabled: Boolean,
  discordId: Option[String],
  discordUsername: Option[String],
) extends WithID derives Decoder

case class SharedLockExtensions/*[Config <: ExtensionConfig]*/(
  slug: String,
  config: ExtensionConfig,
  mode: AvailableModes,
  regularity: FiniteDuration,
  name: String,
) derives Decoder

case class Extension/*[Config <: ExtensionConfig]*/(
  slug: String,
  displayName: String,
  summary: String,
  subtitle: String,
  icon: String,
  config: ExtensionConfig,
  _id: ChasterID,
  mode: AvailableModes,
  regularity: FiniteDuration,
  //userData: Any,
  nbActionsRemaining: Int,
  nextActionDate: Option[String], //Wrong on swagger
  isPartner: Boolean,
) extends WithID derives Decoder

object Extension:
  def unapply(extension: Extension): Option[(ExtensionConfig, AvailableModes, FiniteDuration)] =
    Some((extension.config, extension.mode, extension.regularity))

enum LockStatus:
  case Locked, Unlocked, Deserted

object LockStatus:
  given Decoder[LockStatus] = Decoder[String].emapTry(string => Try(LockStatus.valueOf(string.capitalize)))

case class Lock(
  status: LockStatus,
  _id: ChasterID,
  endDate: Option[String],
  title: String,
  totalDuration: Millis, //The total duration, since the creation of the lock, in miliseconds
  user: User,
  keyholder: Option[User],
  sharedLock: Option[SharedLock],
  isAllowedToViewTime: Boolean,
  isFrozen: Boolean,
  extensions: List[Extension],
  startDate: Instant,
  maxLimitDate: Option[String],
  displayRemainingTime: Boolean,
  limitLockTime: Boolean,
  unlockedAt: Option[Instant], //Wrong on swagger
  frozenAt: Option[Instant],
  hideTimeLogs: Boolean,
  trusted: Boolean,
  isTestLock: Boolean,
) extends WithID derives Decoder

case class SharedLock(
  _id: ChasterID,
  minDuration: FiniteDuration, //Seconds
  maxDuration: FiniteDuration, //Seconds
  calculatedMaxLimitDuration: Option[FiniteDuration],
  user: User,
  requirePassword: Boolean,
  //durationMode: String, //[ duration, date ]
  password: Option[String],
  maxLimitDuration: Option[FiniteDuration],
  minDate: Option[Instant],
  maxDate: Option[Instant],
  maxLimitDate: Option[Instant],
  displayRemainingTime: Boolean,
  limitLockTime: Boolean,
  maxLockedUsers: Option[Int],
  isPublic: Boolean,
  requireContact: Boolean,
  name: String,
  description: String,
  hideTimeLogs: Boolean,
  //Only returned in shared locks endpoints
  extensions: Option[List[SharedLockExtensions]], //Wrong on swagger
  locks: Option[List[String]], //List of locks
) extends WithID derives Decoder

case class Event[T: Decoder](
  extension: Option[String],
  _id: ChasterID,
  `type`: String,
  role: String,
  description: String,
  createdAt: Instant,
  user: Option[User],
  payload: T
) extends WithID:
  def as[TT: Decoder](using ev: T =:= Json): Option[Event[TT]] =
    ev(payload).as[TT].toOption.map(Event(extension, _id, `type`, role, description, createdAt, user, _))

object Event:
  inline given decoder[T: Decoder]: Decoder[Event[T]] = (c: HCursor) =>
    for
      extension <- c.get[Option[String]]("extension")
      id <- c.get[ChasterID]("_id")
      `type` <- c.get[String]("type")
      role <- c.get[String]("role")
      description <- c.get[String]("description")
      createdAt <- c.get[Instant]("createdAt")
      user <- c.get[Option[User]]("user")
      payload <- c.get[T]("payload")
    yield Event(extension, id, `type`, role, description, createdAt, user, payload)

enum SegmentType:
  case AddTime, RemoveTime, AddRemoveTime, Text, SetFreeze, SetUnfreeze, Pillory, Freeze

object SegmentType:
  given Decoder[SegmentType] = Decoder[String].emapTry { string =>
    Try(SegmentType.valueOf(string.kebabToPascalCase))
  }
  given Encoder[SegmentType] = Encoder[String].contramap(_.toString.pascalToKebabCase)

case class Segment(
  text: String,
  `type`: SegmentType = SegmentType.Text,
  duration: FiniteDuration = 1.hour,
) derives Decoder, Encoder.AsObject

case class WheelTurnedPayload(
  segment: Segment
) derives Decoder

case class Data (
  voteEndsAt: Instant,
  createdAt: Option[Instant]
) derives Decoder

case class Post(
  _id: ChasterID,
  lock: Lock,
  `type`: String,
  user: User,
  data: Data
) extends WithID derives Decoder

case class KeyholderLockSearch(
  status: String,
  name: String,
) derives Encoder.AsObject

enum AvailableModes:
  case Unlimited, Cumulative, NonCumulative, Turn

object AvailableModes:
  given Decoder[AvailableModes] = Decoder[String].emapTry { string =>
    Try(AvailableModes.valueOf(string.snakeToPascalCase))
  }
  given Encoder[AvailableModes] = Encoder[String].contramap(_.toString.pascalToSnakeCase)

case class ExtensionListing(
  subtitle: String,
  summary: String,
  displayName: String,
  icon: String,
  slug: String,
  availableModes: List[AvailableModes],
  defaultConfig: ExtensionConfig,
  defaultRegularity: FiniteDuration,
  isEnabled: Boolean,
  isPremium: Boolean,
  isCountedInExtensionsLimit: Boolean,
  isPartner: Boolean,
  isFeatured: Boolean,
  isTesting: Boolean,
  hasActions: Boolean,

  configIframeUrl: Option[String], //Only on cards
  partnerExtensionId: Option[String], //Only on cards
) derives Decoder

case class FrequencyParam(
  nbActions: Int,
  frequency: FiniteDuration,
) derives Decoder, Encoder.AsObject

case class TimeLimitParam(
  timeLimit: FiniteDuration,
) derives Decoder, Encoder.AsObject

case class DurationParam(
  duration: FiniteDuration,
) derives Decoder, Encoder.AsObject

sealed trait Punishment

case class AddTimePunishment(params: FiniteDuration) extends Punishment derives Decoder, Encoder.AsObject

case object FreezePunishment extends Punishment derives Decoder, Encoder.AsObject

case class PilloryPunishment(params: DurationParam) extends Punishment derives Decoder, Encoder.AsObject

object Punishment:
  given Encoder[Punishment] = punishment =>
    val partialEncoder: Encoder[Punishment] = Encoder.instance {
      case punishment: AddTimePunishment => punishment.asJson
      case punishment: FreezePunishment.type => punishment.asJson
      case punishment: PilloryPunishment => punishment.asJson
    }
    val name = punishment.getClass.getSimpleName.split("\\$").last.replace("Punishment", "").pascalToSnakeCase
    partialEncoder.apply(punishment).mapObject(_.add("name", name.asJson))

  given Decoder[Punishment] = cursor =>
    cursor.get[String]("name").flatMap {
      case "add_time" => cursor.get[FiniteDuration]("params").map(AddTimePunishment.apply)
      case "freeze" => Right(FreezePunishment)
      case "pillory" => cursor.get[DurationParam]("params").map(PilloryPunishment.apply)
    }

sealed trait PunishmentConfig

object PunishmentConfig:
  given Encoder[PunishmentConfig] = punishmentConfig =>
    val punishmentConfigEncoder: Encoder[PunishmentConfig] = Encoder.instance {
      case punishment: WheelOfFortuneTurnsPunishmentConfig => punishment.asJson
      case punishment: DiceRollPunishmentConfig            => punishment.asJson
      case punishment: TasksPunishmentConfig               => punishment.asJson
      case punishment: TasksDoTaskPunishmentConfig         => punishment.asJson
      case punishment: TemporaryOpeningOpenPunishmentConfig      => punishment.asJson
      case punishment: TemporaryOpeningTimeLimitPunishmentConfig => punishment.asJson
      case punishment: VerificationPictureVerifyPunishmentConfig => punishment.asJson
    }
    val name = punishmentConfig.getClass.getSimpleName.split("\\$").last.replace("PunishmentConfig", "").pascalToSnakeCase
    punishmentConfigEncoder.apply(punishmentConfig).mapObject(_.add("name", name.asJson).add("prefix", "default".asJson))

  given Decoder[PunishmentConfig] = cursor =>
    lazy val frequencyParam = cursor.get[FrequencyParam]("params")
    lazy val timeLimitParam = cursor.get[TimeLimitParam]("params")
    cursor.get[List[Punishment]]("punishments").flatMap { punishments =>
      cursor.get[String]("name").flatMap {
        case "wheel_of_fortune_turns" => frequencyParam.map(WheelOfFortuneTurnsPunishmentConfig.apply(_, punishments))
        case "dice_roll" => frequencyParam.map(DiceRollPunishmentConfig.apply(_, punishments))
        case "tasks" => frequencyParam.map(TasksPunishmentConfig.apply(_, punishments))
        case "tasks_do_task" => timeLimitParam.map(TasksDoTaskPunishmentConfig.apply(_, punishments))
        case "temporary_opening_open" => frequencyParam.map(TemporaryOpeningOpenPunishmentConfig.apply(_, punishments))
        case "temporary_opening_time_limit" => timeLimitParam.map(TemporaryOpeningTimeLimitPunishmentConfig.apply(_, punishments))
        case "verification_picture_verify" => frequencyParam.map(VerificationPictureVerifyPunishmentConfig.apply(_, punishments))
      }
    }

case class WheelOfFortuneTurnsPunishmentConfig(
  params: FrequencyParam,
  punishments: List[Punishment],
) extends PunishmentConfig derives Decoder, Encoder.AsObject

case class DiceRollPunishmentConfig(
  params: FrequencyParam,
  punishments: List[Punishment],
) extends PunishmentConfig derives Decoder, Encoder.AsObject

case class TasksPunishmentConfig(
  params: FrequencyParam,
  punishments: List[Punishment],
) extends PunishmentConfig derives Decoder, Encoder.AsObject

case class TasksDoTaskPunishmentConfig(
  params: TimeLimitParam,
  punishments: List[Punishment],
) extends PunishmentConfig derives Decoder, Encoder.AsObject

case class TemporaryOpeningOpenPunishmentConfig(
  params: FrequencyParam,
  punishments: List[Punishment],
) extends PunishmentConfig derives Decoder, Encoder.AsObject

case class TemporaryOpeningTimeLimitPunishmentConfig(
  params: TimeLimitParam,
  punishments: List[Punishment],
) extends PunishmentConfig derives Decoder, Encoder.AsObject

case class VerificationPictureVerifyPunishmentConfig(
  params: FrequencyParam,
  punishments: List[Punishment],
) extends PunishmentConfig derives Decoder, Encoder.AsObject

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
  limitToLoggedUsers: Boolean
) extends ExtensionConfig derives Decoder, Encoder.AsObject

case class PilloryConfig(
  timeToAdd: FiniteDuration,
  limitToLoggedUsers: Boolean,
) extends ExtensionConfig derives Decoder, Encoder.AsObject

case class DiceConfig(
  multiplier: FiniteDuration,
) extends ExtensionConfig derives Decoder, Encoder.AsObject

case class WheelOfFortuneConfig(
  segments: List[Segment],
) extends ExtensionConfig derives Decoder, Encoder.AsObject

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
) extends ExtensionConfig derives Decoder, Encoder.AsObject

case class PenaltyConfig(
  penalties: List[PunishmentConfig]
) extends ExtensionConfig derives Decoder, Encoder.AsObject

case class TemporaryOpeningConfig(
  openingTime: FiniteDuration,
  penaltyTime: FiniteDuration,
  allowOnlyKeyholderToOpen: Boolean,
) extends ExtensionConfig derives Decoder, Encoder.AsObject

case class PeerVerification (
  enabled: Boolean,
  punishments: List[Punishment]
) derives Decoder, Encoder.AsObject

case class VerificationPictureConfig(
  visibility: String,
  peerVerification: PeerVerification
) extends ExtensionConfig derives Decoder, Encoder.AsObject

enum RandomEventsDifficulty:
  case Easy, Normal, Hard, Expert

object RandomEventsDifficulty:
  given Decoder[RandomEventsDifficulty] = Decoder[String].emapTry { string =>
    Try(RandomEventsDifficulty.valueOf(string.capitalize))
  }
  given Encoder[RandomEventsDifficulty] = Encoder[String].contramap(_.toString.toLowerCase)

case class RandomEventsConfig(
  difficulty: RandomEventsDifficulty,
) extends ExtensionConfig derives Decoder, Encoder.AsObject

case class GuessTimerConfig(
  minRandomTime: FiniteDuration,
  maxRandomTime: FiniteDuration,
) extends ExtensionConfig derives Decoder, Encoder.AsObject

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
) extends ExtensionConfig derives Decoder, Encoder.AsObject

object PlayCardsConfig:
  def apply(regularity: FiniteDuration, nbKeysRequired: Int, cards: Card*): PlayCardsConfig =
    new PlayCardsConfig(regularity, AvailableModes.Unlimited, nbKeysRequired, cards.toList)

//StayLockedTheWholeMonthConfig has no config aparently
/*case class StayLockedTheWholeMonthConfig(
) extends ExtensionConfig*/

case class ConfigUpdate[+Config <: ExtensionConfig](
  config: Config,
  mode: AvailableModes,
  regularity: FiniteDuration,
)

object ConfigUpdate:
  given Encoder[ConfigUpdate[ExtensionConfig]] = configUpdate => Json.obj(
    "slug" -> configUpdate.config.getClass.getSimpleName.replace("Config", "").pascalToKebabCase.asJson,
    "config" -> configUpdate.config.asJson,
    "mode" -> configUpdate.mode.asJson,
    "regularity" -> configUpdate.regularity.asJson,
  )

case class ConfigUpdatePayload(
  extensions: List[ConfigUpdate[ExtensionConfig]]
) derives Encoder.AsObject

case class SettingsUpdate(
  displayRemainingTime: Boolean,
  hideTimeLogs: Boolean,
) derives Encoder.AsObject

case class SharedLink(
  lockId: ChasterID,
  extensionId: ChasterID,
  votes: Int,
  minVotes: Int,
  canVote: Boolean,
) derives Decoder

enum VoteAction:
  case Add, Remove, Random

object VoteAction:
  given Decoder[VoteAction] = Decoder[String].emapTry { string =>
    Try(VoteAction.valueOf(string.capitalize))
  }
  given Encoder[VoteAction] = Encoder[String].contramap(_.toString.toLowerCase)


sealed trait ExtensionActionPayload
object ExtensionActionPayload:
/*  given Decoder[ExtensionActionPayload] = List[Decoder[ExtensionActionPayload]](
    Decoder[VotePayload].widen,
  ).reduceLeft(_.or(_))*/

  given Encoder[ExtensionActionPayload] = Encoder.instance {
    case payload: VotePayload => payload.asJson
  }


case class ExtensionAction[+Payload <: ExtensionActionPayload: Encoder](
  action: String,
  payload: Payload
)

object ExtensionAction:
  given Encoder[ExtensionAction[ExtensionActionPayload]] = extensionAction => Json.obj(
    "action" -> extensionAction.action.asJson,
    "payload" -> extensionAction.payload.asJson,
  )

case class VotePayload(
  action: VoteAction,
  sessionId: ChasterID,
) extends ExtensionActionPayload derives Encoder.AsObject

case class VoteResponse(
  duration: FiniteDuration,
) derives Decoder