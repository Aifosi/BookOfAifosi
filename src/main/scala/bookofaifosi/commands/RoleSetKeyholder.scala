package bookofaifosi.commands

import bookofaifosi.db.Filters.*
import bookofaifosi.db.{RegisteredUserRepository, UserRoleRepository, given}
import bookofaifosi.model.event.{AutoCompleteEvent, SlashCommandEvent}
import bookofaifosi.model.{Role, toLong}
import cats.effect.IO
import cats.syntax.foldable.*
import cats.syntax.option.*
import cats.syntax.traverse.*
import doobie.syntax.string.*

object RoleSetKeyholder extends SlashCommand with Options:
  override val defaultEnabled: Boolean = false
  override val fullCommand: String = "role set keyholder"
  override val options: List[PatternOption] = List(
    _.addOption[Role]("role", "Role to add to keyholders.")
  )

  override def apply(pattern: SlashPattern, event: SlashCommandEvent): IO[Boolean] =
    val role = event.getOption[Role]("role")
    for
      keyholders <- RegisteredUserRepository.list(isKeyholder)
      guild = event.guild.get
      existingUserRole <- UserRoleRepository.find(guild.discordID.equalGuildID, role.discordID.equalRoleID, fr"user_type = 'keyholder'".some)
      _ <- existingUserRole.traverse_(userRole => keyholders.traverse_(_.removeRole(guild, userRole.role)))
      _ <- UserRoleRepository.addOrUpdate(guild.discordID, role.discordID, "keyholder")
      _ <- existingUserRole.traverse_(userRole => keyholders.traverse_(_.addRole(guild, role)))
      _ <- event.replyEphemeral(s"${role.mention} added to users currently registered as keyholders.")
    yield true

  override val description: String = "Sets a role to be added to all current and new keyholders"
