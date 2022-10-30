package lurch.model

import bot.model.{ChasterID, Guild, RegisteredUser}

import java.time.Instant
import java.util.UUID

case class PilloryLink(
  user: RegisteredUser,
  guild: Guild,
  postID: ChasterID,
  counted: Boolean,
)
