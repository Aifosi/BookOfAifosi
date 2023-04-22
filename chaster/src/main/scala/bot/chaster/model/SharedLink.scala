package bot.chaster.model

import bot.model.ChasterID

import io.circe.Decoder

case class SharedLink(
  lockId: ChasterID,
  extensionId: ChasterID,
  votes: Int,
  minVotes: Int,
  canVote: Boolean,
) derives Decoder
