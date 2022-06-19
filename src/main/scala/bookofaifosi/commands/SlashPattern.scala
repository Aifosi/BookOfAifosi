package bookofaifosi.commands

import cats.syntax.option.*
import net.dv8tion.jda.api.interactions.commands.OptionType
import net.dv8tion.jda.api.interactions.commands.build.{Commands, SlashCommandData, SubcommandData}

import scala.quoted.{Quotes, Type}

case class SlashPattern(
  name: String,
  description: String,
  subCommands: Set[String] = Set.empty,
  commandOptions: List[SlashCommandData => SlashCommandData] = List.empty,
  subCommandOptions: Map[String, List[SubcommandData => SubcommandData]] = Map.empty,
):
  lazy val build: SlashCommandData =
    val command: SlashCommandData = Commands.slash(name, description)
    if subCommands.isEmpty then
      commandOptions.foldLeft(command)((command, option) => option(command))
    else
      subCommands.foldLeft(command) { (command, subCommandName) =>
        val subCommand = subCommandOptions.getOrElse(subCommandName, List.empty).foldLeft(new SubcommandData(subCommandName, description))((command, option) => option(command))
        command.addSubcommands(subCommand)
      }

  inline def addOption[T](name: String, description: String, autoComplete: Boolean = false): Option[String] => SlashPattern = (subCommand: Option[String]) =>
    require(subCommand.forall(subCommands.contains) , s"Trying to add option to nonexistent subcommand $subCommand")
    subCommand.fold {
      val option: SlashCommandData => SlashCommandData = SlashPatternHelper.addOption[T](_, name, description, autoComplete)
      copy(commandOptions = commandOptions :+ option)
    } { subCommand =>
      val option: SubcommandData => SubcommandData = SlashPatternHelper.addSubCommandOption[T](_, name, description, autoComplete)
      copy(subCommandOptions = subCommandOptions.updatedWith(subCommand)(_.fold(List(option))(_ :+ option).some))
  }

  override def toString: String = s"/$name ${subCommands.mkString(" ")}"

  def merge(other: SlashPattern): SlashPattern =
    require(name == other.name, s"Trying to merge two unrelated slash commands $name and ${other.name}")
    //require(subCommands.nonEmpty && other.subCommands.nonEmpty, s"Both commands must have sub commands to be merged $name and ${other.name}")
    copy(
      description = if subCommands.isEmpty then description else other.description,
      subCommands = subCommands ++ other.subCommands,
      commandOptions = commandOptions ++ other.commandOptions,
      subCommandOptions = subCommandOptions ++ other.subCommandOptions
    )
