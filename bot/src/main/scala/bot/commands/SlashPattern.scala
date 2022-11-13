package bot.commands

import cats.syntax.option.*
import net.dv8tion.jda.api.interactions.commands.OptionType
import net.dv8tion.jda.api.interactions.commands.build.{Commands, SlashCommandData, SubcommandData, SubcommandGroupData}

import scala.quoted.{Quotes, Type}

case class SlashPattern(
  name: String,
  subCommandGroup: Option[String],
  subCommand: Option[String],
  description: String,
  defaultEnabled: Boolean,
  commandOptions: List[SlashCommandData => SlashCommandData] = List.empty,
  subCommandOptions: List[SubcommandData => SubcommandData] = List.empty,
):
  inline def addOption[T](name: String, description: String, autoComplete: Boolean = false): SlashPattern =
    subCommand.fold {
      val option: SlashCommandData => SlashCommandData = MacroHelper.addOption[T](_, name, description, autoComplete)
      copy(commandOptions = commandOptions :+ option)
    } { subCommand =>
      val option: SubcommandData => SubcommandData = MacroHelper.addSubCommandOption[T](_, name, description, autoComplete)
      copy(subCommandOptions = subCommandOptions :+ option)
    }

object SlashPattern:
  extension [A](command: A)
    def applyOptions(options: List[A => A]) = options.foldLeft(command)((command, option) => option(command))

  def buildCommands(commands: List[SlashPattern]): List[SlashCommandData] =
    commands.groupBy(_.name).view.mapValues(_.groupBy(_.subCommandGroup).view.mapValues(_.groupBy(_.subCommand)).toMap).toList.map {
      case (commandName, other) =>
        lazy val defaultEnabled = other.values.flatMap(_.values.flatten).toList.forall(_.defaultEnabled)
        val command = other.get(None).flatMap(_.get(None)).flatMap(_.headOption)
          .fold(Commands.slash(commandName, "No description")) { pattern =>
            Commands.slash(commandName, pattern.description).applyOptions(pattern.commandOptions)
          }

        other.foldLeft(command) {
          case (command, (None, other))                                              => other.foldLeft(command) {
            case (command, (None, _)) => command
            case (command, (Some(subCommandName), List(pattern))) =>
              //Commands with subCommand only
              val subCommand = new SubcommandData(subCommandName, pattern.description).applyOptions(pattern.subCommandOptions)
              command.addSubcommands(subCommand)

            case _ => throw new Exception("Unexpected number of patterns")
          }
          case (command, (Some(subCommandGroupName), other)) if other.contains(None) => throw new Exception(s"$commandName $subCommandGroupName has no subCommands")
          case (command, (Some(subCommandGroupName), other))                         =>
            //Commands with subCommandGroup and subCommand
            val subcommandGroup = other.view.collect { case (Some(subCommandName), other) => subCommandName -> other }
              .foldLeft(new SubcommandGroupData(subCommandGroupName, "No description")) {
                case (subcommandGroup, (subCommandName, List(pattern))) =>
                  val subCommand = new SubcommandData(subCommandName, pattern.description).applyOptions(pattern.subCommandOptions)
                  subcommandGroup.addSubcommands(subCommand)

                case _ => throw new Exception("Unexpected number of patterns")
              }
            command.addSubcommandGroups(subcommandGroup)
        }.setDefaultEnabled(defaultEnabled)
    }
