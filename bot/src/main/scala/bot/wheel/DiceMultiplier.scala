package bot.wheel

import bot.DiscordLogger
import bot.chaster.ChasterClient
import bot.chaster.model.{ConfigUpdate, DiceConfig, LinkConfig}
import bot.db.RegisteredUserRepository
import bot.model.{ChasterID, RegisteredUser}
import bot.syntax.io.*
import bot.tasks.ModifierTextWheelCommand
import bot.tasks.ModifierTextWheelCommand.Modifier

import cats.data.OptionT
import cats.effect.IO
import cats.syntax.functor.*
import org.typelevel.log4cats.Logger
import scala.concurrent.duration.*
import scala.reflect.Typeable

class DiceMultiplier(
  client: ChasterClient,
  registeredUserRepository: RegisteredUserRepository,
)(using DiscordLogger)
    extends ModifierTextWheelCommand[DiceConfig](client, registeredUserRepository):
  override def textPattern: String = "DiceMultiplier:"
  override def logName: String     = "dice multiplier"

  override def configUpdate(configUpdate: ConfigUpdate[DiceConfig], modifier: Modifier)(using
    Typeable[DiceConfig],
  ): ConfigUpdate[DiceConfig] =
    configUpdate.copy(
      config = configUpdate.config.copy(
        multiplier = modifier.apply(configUpdate.config.multiplier.toMinutes.toInt).minutes,
      ),
    )

  override val description: String = "Changes the dice multiplier"
