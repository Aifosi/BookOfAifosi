package bookofaifosi.model.event

import net.dv8tion.jda.api.entities.MessageChannel
import net.dv8tion.jda.api.entities.{MessageChannel, Guild as JDAGuild, Member as JDAMember, User as JDAUser}

abstract class GenericTextEvent(
  jdaChannel: MessageChannel,
  jdaAuthor: JDAUser,
  jdaMember: Option[JDAMember],
  jdaGuild: Option[JDAGuild],
) extends Event(jdaChannel, jdaAuthor, jdaMember, jdaGuild):
  lazy val authorName: String = jdaMember.flatMap(m => Option(m.getNickname)).getOrElse(author.name)
