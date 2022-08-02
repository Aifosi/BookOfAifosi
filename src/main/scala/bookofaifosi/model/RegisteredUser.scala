package bookofaifosi.model

import bookofaifosi.db.User as DBUser

import java.time.Instant
import java.util.UUID

class RegisteredUser(
  dbUser: DBUser,
  user: User,
  val token: UserToken,
) extends User(user.user):
  val id: UUID = dbUser.id
  val chasterID: Option[ChasterID] = dbUser.chasterID
  val keyholderIDs: List[ChasterID] = dbUser.keyholderIDs
  val isLocked: Boolean = dbUser.isLocked
  val isWearer: Boolean = dbUser.isWearer
  val isKeyholder: Boolean = dbUser.isKeyholder
  val lastLocked: Option[Instant] = dbUser.lastLocked
  val lastKeyheld: Option[Instant] = dbUser.lastKeyheld
