package bookofaifosi.model

import bookofaifosi.db.User as DBUser

import java.time.Instant
import java.util.UUID

class RegisteredUser(
  dbUser: DBUser,
  member: Member,
  val token: UserToken,
) extends Member(member.member):
  val id: UUID = dbUser.id
  val chasterID: ChasterID = dbUser.chasterID
  val keyholderIDs: List[ChasterID] = dbUser.keyholderIDs
  val isLocked: Boolean = dbUser.isLocked
  val lastLocked: Option[Instant] = dbUser.lastLocked
  val lastKeyheld: Option[Instant] = dbUser.lastKeyheld

  override lazy val toString: String = member.toString
