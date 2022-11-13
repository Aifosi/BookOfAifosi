package lurch.model

import bot.model.{ChasterID, RegisteredUser}

import java.time.Instant

case class RecentLockHistory(
  registeredUser: RegisteredUser,
  lockID: ChasterID,
  mostRecentEventTime: Option[Instant],
)
