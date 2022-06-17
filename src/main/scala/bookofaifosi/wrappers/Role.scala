package bookofaifosi.wrappers

import net.dv8tion.jda.api.entities.Role as JDARole

class Role(private[wrappers] val role: JDARole):
  lazy val id = role.getIdLong
  lazy val name = role.getName
