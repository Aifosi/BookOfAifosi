package bookofaifosi.model

import net.dv8tion.jda.api.entities.Role as JDARole

class Role(private[model] val role: JDARole):
  lazy val discordID = role.getIdLong
  lazy val name = role.getName
