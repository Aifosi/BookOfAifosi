package bookofaifosi.model

import net.dv8tion.jda.api.entities.Role as JDARole

class Role(private[model] val role: JDARole):
  lazy val discordID: DiscordID = role.getIdLong
  lazy val name: String = role.getName
  lazy val mention: String = role.getAsMention

  override def toString: String = name
