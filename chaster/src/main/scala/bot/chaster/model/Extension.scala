package bot.chaster.model

import bot.chaster.model.{AvailableModes, ExtensionConfig, WithID}
import bot.chaster.model.instances.given
import bot.model.ChasterID

import io.circe.Decoder
import io.circe.syntax.*
import scala.concurrent.duration.FiniteDuration

case class SharedLockExtensions /*[Config <: ExtensionConfig]*/ (
  slug: String,
  config: ExtensionConfig,
  mode: AvailableModes,
  regularity: FiniteDuration,
  name: String,
) derives Decoder

case class Extension /*[Config <: ExtensionConfig]*/ (
  slug: String,
  displayName: String,
  summary: String,
  subtitle: String,
  icon: String,
  config: ExtensionConfig,
  _id: ChasterID,
  mode: AvailableModes,
  regularity: FiniteDuration,
  // userData: Any,
  nbActionsRemaining: Int,
  nextActionDate: Option[String], // Wrong on swagger
  isPartner: Boolean,
) extends WithID
    derives Decoder

object Extension:
  def unapply(extension: Extension): Option[(ExtensionConfig, AvailableModes, FiniteDuration)] =
    Some((extension.config, extension.mode, extension.regularity))
