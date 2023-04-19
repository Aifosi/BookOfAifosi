package bot.model

import bot.chaster.ChasterClient
import bot.db.Filters.*
import cats.effect.IO

import java.time.Instant
import java.util.UUID

class RegisteredUser(
  val id: UUID,
  val chasterID: ChasterID,
  val guildID: DiscordID,
  val keyholderIDs: List[ChasterID],
  val isLocked: Boolean,
  val lastLocked: Option[Instant],
  val lastKeyheld: Option[Instant],
  member: Member,
  val tokenGetter: IO[UserToken],
  val tokenID: UUID,
) extends Member(member.member):
  
  def authenticatedEndpoints(client: ChasterClient): IO[ChasterClient#AuthenticatedEndpoints] =
    tokenGetter.map(client.authenticatedEndpoints)

  override lazy val toString: String = member.toString
