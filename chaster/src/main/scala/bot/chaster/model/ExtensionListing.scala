package bot.chaster.model

import bot.chaster.model.ExtensionConfig
import bot.chaster.model.instances.given

import io.circe.Decoder
import scala.concurrent.duration.FiniteDuration

case class ExtensionListing(
  subtitle: String,
  summary: String,
  displayName: String,
  icon: String,
  slug: String,
  availableModes: List[AvailableModes],
  defaultConfig: ExtensionConfig,
  defaultRegularity: FiniteDuration,
  isEnabled: Boolean,
  isPremium: Boolean,
  isCountedInExtensionsLimit: Boolean,
  isPartner: Boolean,
  isFeatured: Boolean,
  isTesting: Boolean,
  hasActions: Boolean,
  configIframeUrl: Option[String],   // Only on cards
  partnerExtensionId: Option[String],// Only on cards
) derives Decoder
