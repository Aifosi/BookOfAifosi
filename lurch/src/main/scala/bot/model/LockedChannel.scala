package bot.model

import bot.model.{Channel, Guild}

case class LockedChannel(
  guild: Guild,
  channel: Channel,
)
