package bot.chaster

import bot.model.ChasterID
import io.circe.{Decoder, Encoder, HCursor, Json}

import java.time.Instant
import scala.deriving.Mirror
import scala.util.Try
import scala.util.chaining.*

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

case class User (
  _id: ChasterID,
  username: String,
  gender: Option[String],
  role: String,
  discordId: Option[String],
  discordUsername: Option[String],
) extends WithID derives Decoder

case class PublicUser (
  _id: ChasterID,
  username: String,
  gender: Option[String],
  isDisabled: Boolean,
  discordId: Option[String],
  discordUsername: Option[String],
) extends WithID derives Decoder

case class SharedLockExtensions (
  slug: String,
  name: String,
  textConfig: String,
  mode: String,
  regularity: Int
) derives Decoder

case class Extensions (
  slug: String,
  //config: Any,
  _id: ChasterID,
  displayName: String,
  summary: String,
  subtitle: String,
  icon: String,
  mode: String,
  regularity: Int,
  //userData: Any,
  nbActionsRemaining: Int,
  nextActionDate: Option[String], //Wrong on swagger
) extends WithID derives Decoder

enum LockStatus:
  case Locked, Unlocked, Deserted

object LockStatus:
  given Decoder[LockStatus] = Decoder[String].emapTry(string => Try(LockStatus.valueOf(string.capitalize)))

case class Lock(
  status: LockStatus,
  _id: ChasterID,
  endDate: Option[String],
  title: String,
  totalDuration: Long, //The total duration, since the creation of the lock, in seconds
  user: User,
  keyholder: Option[User],
  sharedLock: Option[SharedLock],
  isAllowedToViewTime: Boolean,
  isFrozen: Boolean,
  extensions: List[Extensions],
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

case class SharedLock (
  _id: ChasterID,
  minDuration: Long, //Seconds
  maxDuration: Long, //Seconds
  calculatedMaxLimitDuration: Option[Long],
  user: User,
  requirePassword: Boolean,
  //durationMode: String, //[ duration, date ]
  password: Option[String],
  maxLimitDuration: Option[Long],
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

case class Event[T: Decoder] (
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

object Event {
  inline given decoder[T: Decoder]: Decoder[Event[T]] = (c: HCursor) =>
    for
      extension <- c.downField("extension").as[Option[String]]
      id <- c.downField("_id").as[ChasterID]
      `type` <- c.downField("type").as[String]
      role <- c.downField("role").as[String]
      description <- c.downField("description").as[String]
      createdAt <- c.downField("createdAt").as[Instant]
      user <- c.downField("user").as[Option[User]]
      payload <- c.downField("payload").as[T]
    yield Event(extension, id, `type`, role, description, createdAt, user, payload)
}

case class Segment(
  `type`: String,
  duration: Int,
  text: String
) derives Decoder

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
