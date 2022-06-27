package bookofaifosi.model

import cats.effect.IO
import net.dv8tion.jda.api.JDA
import bookofaifosi.syntax.action.*
import cats.data.OptionT
import scala.jdk.CollectionConverters.*
import net.dv8tion.jda.api.entities.MessageChannel

class Discord(jda: JDA):
  def userByID(id: DiscordID): IO[User] =
    OptionT.fromOption(Option(jda.getUserById(id.toLong))).getOrElseF(jda.retrieveUserById(id.toLong).toIO).map(new User(_))
  def guildByID(id: DiscordID): IO[Guild] =
    IO.fromOption(Option(jda.getGuildById(id.toLong)))(new Exception(s"Failed to get guild with id $id")).map(new Guild(_))
  def roleByID(id: DiscordID): IO[Role] =
    IO.fromOption(Option(jda.getRoleById(id.toLong)))(new Exception(s"Failed to get role with id $id")).map(new Role(_))
  def channelByID(id: DiscordID): IO[Channel] =
    IO.fromOption(Option(jda.getChannelById(classOf[MessageChannel], id.toLong)))(new Exception(s"Failed to get channel with id $id")).map(new Channel(_))

  def roles(guildID: DiscordID): IO[List[Role]] = guildByID(guildID.toLong).map(_.roles)

  jda.retrieveApplicationInfo()
