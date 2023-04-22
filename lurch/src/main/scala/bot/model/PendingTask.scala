package bot.model

import bot.Bot
import bot.Lurch
import bot.model.{Channel, DiscordID, Message, RegisteredUser}

import cats.data.OptionT
import cats.effect.IO
import cats.effect.kernel.Deferred
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

  def message(tortureChamberChannel: Deferred[IO, Option[Channel]]): OptionT[IO, Message] =
    OptionT(tortureChamberChannel.get).flatMap(_.findMessageByID(messageID))
