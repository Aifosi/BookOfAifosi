package bookofaifosi.model

import net.dv8tion.jda.api.entities.User as DiscordUser
import bookofaifosi.db.User as DBUser

import java.time.Instant
import java.util.UUID

class RegisteredUser(
  user: DiscordUser,
  dbUser: DBUser,
) extends User(user):
  val id: UUID = dbUser.id
  val chasterName: String = dbUser.chasterName
  val accessToken: String = dbUser.accessToken
  val expiresAt: Instant = dbUser.expiresAt
  val refreshToken: String = dbUser.refreshToken
  val scope: String = dbUser.scope
