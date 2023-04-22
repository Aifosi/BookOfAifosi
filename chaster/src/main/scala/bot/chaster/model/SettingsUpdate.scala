package bot.chaster.model

import io.circe.Encoder

case class SettingsUpdate(
  displayRemainingTime: Boolean,
  hideTimeLogs: Boolean,
) derives Encoder.AsObject
