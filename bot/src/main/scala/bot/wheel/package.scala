package bot.wheel

import bot.DiscordLogger
import bot.chaster.ChasterClient
import bot.db.RegisteredUserRepository
import bot.tasks.WheelCommand

import cats.data.NonEmptyList

def commonWheelCommands(
  chasterClient: ChasterClient,
  registeredUserRepository: RegisteredUserRepository,
)(using DiscordLogger): NonEmptyList[WheelCommand[?]] = NonEmptyList.of(
  new Once(chasterClient, registeredUserRepository),
  new OnceGroup(chasterClient, registeredUserRepository),
  // These two need to be before other commands
  new DiceMultiplier(chasterClient, registeredUserRepository),
  new VerificationPictures(chasterClient, registeredUserRepository),
  new PilloryVoteTime(chasterClient, registeredUserRepository),
  new DiceRolls(chasterClient, registeredUserRepository),
  new WheelRolls(chasterClient, registeredUserRepository),
  new VoteTarget(chasterClient, registeredUserRepository),
  new VoteAdd(chasterClient, registeredUserRepository),
  new VoteRemove(chasterClient, registeredUserRepository),
  new AddSegments(chasterClient, registeredUserRepository),
  new LogTimesHide(chasterClient, registeredUserRepository),
  new LogTimesShow(chasterClient, registeredUserRepository),
  new LogTimesToggle(chasterClient, registeredUserRepository),
  new TimerHide(chasterClient, registeredUserRepository),
  new TimerShow(chasterClient, registeredUserRepository),
  new TimerToggle(chasterClient, registeredUserRepository),
)
