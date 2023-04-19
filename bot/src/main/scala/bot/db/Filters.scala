package bot.db

import bot.model.{ChasterID, DiscordID, Member, given}
import cats.syntax.option.*
import doobie.*
import doobie.implicits.*
import doobie.postgres.*
import doobie.postgres.implicits.*

import java.util.UUID

object Filters:

  extension (id: UUID)
    def equalID: Filter = fr"id = $id".some
    def equalUserID: Filter = fr"user_id = $id".some

  extension (id: Option[UUID])
    def equalID: Filter = id.flatMap(_.equalID)

  extension (member: Member)
    //def equalDiscordAndGuildID = List(member.discordID.equalDiscordID, member.guild.discordID.equalGuildID)
    def equalDiscordAndGuildID =
      fr"user_discord_id = ${member.discordID} and guild_discord_id = ${member.guild.discordID}".some
  
  extension (id: DiscordID)
    def equalDiscordID: Filter = fr"user_discord_id = $id".some
    def equalGuildID: Filter = fr"guild_discord_id = $id".some
    def equalRoleID: Filter = fr"role_discord_id = $id".some
    def equalChannelID: Filter = fr"channel_discord_id = $id".some
    def equalMessageID: Filter = fr"message_discord_id = $id".some

  extension (id: ChasterID)
    def equalChasterID: Filter = fr"chaster_id = $id".some
    def equalLockID: Filter = fr"lock_id = $id".some

  extension (string: String)
    def similarName: Filter = fr"name ILIKE $string".some
    def similarPartialName: Filter = fr"name ILIKE ${s"%$string%"}".some
    def equalName: Filter = fr"name = $string".some
    def equalUserType: Filter = fr"user_type = $string".some
    def equalAccessToken: Filter = fr"access_token = $string".some

  extension (name: Option[String])
    def similarName: Filter = name.flatMap(_.similarName)
    def similarPartialName: Filter = name.flatMap(_.similarPartialName)
    def equalName: Filter = name.flatMap(_.equalName)

  extension (keyholders: List[ChasterID])
    def anyKeyholder: Filter = fr"chaster_id = ANY ($keyholders)".some

  def descriptionEqual(description: Option[Option[String]]): Filter =
    description.map(description => fr"description = ${description.orNull}")
