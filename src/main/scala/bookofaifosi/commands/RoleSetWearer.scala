package bookofaifosi.commands

import bookofaifosi.db.{RegisteredUserRepository, UserRoleRepository}
import bookofaifosi.db.Filters.*
import bookofaifosi.db.given
import bookofaifosi.model.Role
import bookofaifosi.model.event.{AutoCompleteEvent, SlashCommandEvent}
import cats.effect.IO
import doobie.syntax.string.*
import cats.syntax.option.*
import cats.syntax.traverse.*
import cats.syntax.foldable.*
import bookofaifosi.model.toLong

object RoleSetWearer extends SlashCommand with Options:
  override val defaultEnabled: Boolean = false
  override val fullCommand: String = "role set wearer"
  override val options: List[PatternOption] = List(
    _.addOption[Role]("role", "Role to add to wearers.")
  )

  override def apply(pattern: SlashPattern, event: SlashCommandEvent): IO[Boolean] =
    val role = event.getOption[Role]("role")
    for
      wearers <- RegisteredUserRepository.list(isWearer)
      guild = event.guild.get
      existingUserRole <- UserRoleRepository.find(guild.discordID.equalGuildID, role.discordID.equalRoleID, fr"user_type = 'wearer'".some)
      _ <- existingUserRole.traverse_(userRole => wearers.traverse_(_.removeRole(guild, userRole.role)))
      _ <- UserRoleRepository.addOrUpdate(guild.discordID, role.discordID, "wearer")
      _ <- existingUserRole.traverse_(userRole => wearers.traverse_(_.addRole(guild, role)))
      _ <- event.replyEphemeral(s"${role.mention} added to users currently registered as wearers.")
    yield true

  override val description: String = "Sets a role to be added to all current and new wearers"
