package bookofaifosi.model

import cats.effect.IO
import net.dv8tion.jda.api.JDA
import bookofaifosi.syntax.action.*
import cats.data.OptionT

class Discord(jda: JDA):
  def userByID(id: Long): IO[User] =
    OptionT.fromOption(Option(jda.getUserById(id))).getOrElseF(jda.retrieveUserById(id).toIO).map(new User(_))
