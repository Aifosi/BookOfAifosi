package bot.chaster.model

import bot.model.ChasterID

import io.circe.{Encoder, Json}
import io.circe.syntax.*

sealed trait ExtensionActionPayload
object ExtensionActionPayload:
  /*  given Decoder[ExtensionActionPayload] = List[Decoder[ExtensionActionPayload]](
      Decoder[VotePayload].widen,
    ).reduceLeft(_.or(_))*/

  given Encoder[ExtensionActionPayload] = Encoder.instance {
    case payload: VotePayload       => payload.asJson
    case payload: VotePublicPayload => payload.asJson
  }

case class ExtensionAction[+Payload <: ExtensionActionPayload: Encoder](
  action: String,
  payload: Payload,
)

object ExtensionAction:
  given Encoder[ExtensionAction[ExtensionActionPayload]] = extensionAction =>
    Json.obj(
      "action"  -> extensionAction.action.asJson,
      "payload" -> extensionAction.payload.asJson,
    )

case class VotePayload(
  action: VoteAction,
  sessionId: ChasterID,
) extends ExtensionActionPayload
    derives Encoder.AsObject

case class VotePublicPayload(
  voteId: ChasterID,
) extends ExtensionActionPayload
    derives Encoder.AsObject
