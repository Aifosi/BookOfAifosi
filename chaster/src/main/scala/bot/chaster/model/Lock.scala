package bot.chaster.model

import bot.chaster.model.Millis.Millis
import bot.chaster.model.instances.given
import bot.model.ChasterID

import cats.syntax.functor.*
import io.circe.Decoder
import java.time.Instant
import scala.concurrent.duration.FiniteDuration

case class Lock(
  status: LockStatus,
  _id: ChasterID,
  endDate: Option[String],
  title: String,
  totalDuration: Millis,       // The total duration, since the creation of the lock, in miliseconds
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
  unlockedAt: Option[Instant], // Wrong on swagger
  frozenAt: Option[Instant],
  hideTimeLogs: Boolean,
  trusted: Boolean,
  isTestLock: Boolean,
) extends WithID
    derives Decoder

case class SharedLock(
  _id: ChasterID,
  minDuration: FiniteDuration,                    // Seconds
  maxDuration: FiniteDuration,                    // Seconds
  calculatedMaxLimitDuration: Option[FiniteDuration],
  user: User,
  requirePassword: Boolean,
  // durationMode: String, //[ duration, date ]
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
  // Only returned in shared locks endpoints
  extensions: Option[List[SharedLockExtensions]], // Wrong on swagger
  locks: Option[List[String]],                    // List of locks
) extends WithID
    derives Decoder
