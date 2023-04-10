package shared.wheel

import bot.DiscordLogger
import bot.chaster.{ChasterClient, ConfigUpdate, PilloryConfig}
import bot.db.RegisteredUserRepository
import bot.tasks.ModifierTextWheelCommand

import scala.concurrent.duration.*
import scala.reflect.Typeable

class PilloryVoteTime(
  client: ChasterClient,
  registeredUserRepository: RegisteredUserRepository,
)(using DiscordLogger) extends ModifierTextWheelCommand[PilloryConfig](client, registeredUserRepository):
  override def textPattern: String = "PilloryVoteTime:"

  override def logName: String = "pillory vote time"

  override def configUpdate(configUpdate: ConfigUpdate[PilloryConfig], modifier: ModifierTextWheelCommand.Modifier)(using Typeable[PilloryConfig]): ConfigUpdate[PilloryConfig] =
    configUpdate.copy(
      config = configUpdate.config.copy(
        timeToAdd = modifier.apply(configUpdate.config.timeToAdd.toMinutes.toInt).minutes
      )
    )

