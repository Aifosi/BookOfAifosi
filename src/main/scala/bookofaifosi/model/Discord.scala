package bookofaifosi.model

import cats.effect.IO
import net.dv8tion.jda.api.JDA

class Discord(jda: JDA):
  def userByID(id: Long): IO[User] =
    IO.fromOption(Option(jda.getUserById(id)).map(new User(_)))(new Exception(s"Could not find discord user with id $id"))
