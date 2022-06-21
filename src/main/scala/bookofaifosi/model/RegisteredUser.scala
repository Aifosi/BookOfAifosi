package bookofaifosi.model

import bookofaifosi.db.User as DBUser

import java.time.Instant
import java.util.UUID

class RegisteredUser(
  dbUser: DBUser,
  user: User,
) extends User(user.user):
  val id: UUID = dbUser.id
  val chasterName: String = dbUser.chasterName
  val accessToken: String = dbUser.accessToken
  val expiresAt: Instant = dbUser.expiresAt
  val refreshToken: String = dbUser.refreshToken
  val scope: String = dbUser.scope
  lazy val scopes: Array[String] = scope.split(" ")
  lazy val isWearer: Boolean = scopes.contains("locks")
  lazy val isKeyholder: Boolean = scopes.contains("keyholder")
