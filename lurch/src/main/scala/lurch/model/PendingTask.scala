package lurch.model

import bot.Bot
import bot.model.{DiscordID, Message, RegisteredUser}
import cats.data.OptionT
import cats.effect.IO
import lurch.Lurch

import java.time.Instant
import java.util.UUID

case class PendingTask(
  id: UUID,
  title: String,
  messageID: DiscordID,
  user: RegisteredUser,
  keyholder: RegisteredUser,
  completed: Boolean,
  deadline: Option[Instant],
):
  override def toString: String = s"Task \"$title\""

  lazy val message: OptionT[IO, Message] = Lurch.channels.tortureChamber.flatMap(_.findMessageByID(messageID))
