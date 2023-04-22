package bot.wheel

import bot.DiscordLogger
import bot.chaster.ChasterClient
import bot.chaster.model.{ConfigUpdate, LinkConfig}
import bot.db.RegisteredUserRepository
import bot.model.{ChasterID, RegisteredUser}
import bot.syntax.io.*
import bot.tasks.ModifierTextWheelCommand
import bot.tasks.ModifierTextWheelCommand.Modifier
import bot.tasks.ModifierTextWheelCommand.Modifier.*

import cats.data.OptionT
import cats.effect.IO
import org.typelevel.log4cats.Logger
import scala.concurrent.duration.*
import scala.reflect.Typeable

class VoteAdd(
  client: ChasterClient,
  registeredUserRepository: RegisteredUserRepository,
)(using DiscordLogger)
    extends ModifierTextWheelCommand[LinkConfig](client, registeredUserRepository):
  override def textPattern: String = "VoteAdd:"
  override def logName: String     = "time added per vote"

  override def configUpdate(configUpdate: ConfigUpdate[LinkConfig], modifier: Modifier)(using
    Typeable[LinkConfig],
  ): ConfigUpdate[LinkConfig] =
    configUpdate.copy(
      config = configUpdate.config.copy(
        timeToAdd = modifier.apply(configUpdate.config.timeToAdd.toMinutes.toInt).minutes,
      ),
    )

  override val description: String = "Changes the time added per vote of the shared link"
