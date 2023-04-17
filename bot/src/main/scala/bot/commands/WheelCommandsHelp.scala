package bot.commands

import bot.model.event.SlashCommandEvent
import cats.effect.IO
import org.typelevel.log4cats.Logger

object WheelCommandsHelp extends SlashCommand:
  override val isUserCommand: Boolean = true
  override val fullCommand: String = "wheel commands help"
  override val description: String = "Shows detailed help on how to use wheel commands"

  override def apply(pattern: SlashPattern, event: SlashCommandEvent)(using Logger[IO]): IO[Boolean] =
    val helpMessage = s"""
        |Wheel commands are texts that are placed on slice of a wheel of fortune extension of a chaster lock, once the lockee lands on it they will trigger an action.
        |For the commands to work both the keyholder and lockee must be registered with this bot.
        |
        |Example usage for a few wheel commands:
        |
        |If you add a segment with the text "Once: HideTimer", once your lockee lands on it the timer for the lock will be hidden and the segment will be removed from the wheel
        |
        |With the text "VoteTarget: +5", landing on that action increases the number of required votes for the lock by 5
        |"VoteTarget: -5" decreases the required votes by 5
        |
        |Other modifiers you can use are use the multiply (\\*) and divide (\\\\) operations
        |
        |If you use no modifier a concrete value will be set, with the text "VoteTarget: 5", the number of required votes will be set to 5
        |
        |Finally for a more complex example if you want to use the AddSegments command first use the slash command "/${Compress.fullCommand}" giving it a list of wheel of fortune segments you want to add when the AddSegments is triggered.
        |
        |For example (/${Compress.fullCommand} Once: HideTimer, Once: ShowTimer)
        |The command replies with "AddSegments: H4sIAAAAAAAA/4v2z0tOdS/KLy1QKMnMTS2yUvDITEkNATF1FDDkgjPyy8FysQByHIQpOAAAAA=="
        |
        |If you add the entire reply text to a wheel slice, then when a lockee lands on it, two new slices will be added to the wheel. One slice will be "Once: HideTimer" and the other will be  "Once: ShowTimer".
        |
        |For a complete list of all available wheel commands check /${WheelCommandsList.fullCommand}
        |""".stripMargin

    event.replyEphemeral(helpMessage).as(true)
