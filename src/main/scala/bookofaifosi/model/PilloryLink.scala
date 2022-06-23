package bookofaifosi.model

import java.time.Instant
import java.util.UUID

case class PilloryLink(
  user: RegisteredUser,
  guild: Guild,
  postID: String,
  counted: Boolean,
)
