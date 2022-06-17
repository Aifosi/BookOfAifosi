package bookofaifosi.commands.slash

import net.dv8tion.jda.api.interactions.commands.OptionType
import net.dv8tion.jda.api.interactions.commands.build.{Commands, SlashCommandData}

import scala.quoted.{Quotes, Type}

case class SlashPattern(
  name: String,
  description: String,
  builderOptions: List[SlashCommandData => SlashCommandData] = List.empty,
):
  lazy val build: SlashCommandData =
    builderOptions.foldLeft(Commands.slash(name, description))((command, option) => option(command))

  inline def addOption[T](name: String, description: String, autoComplete: Boolean = false): SlashPattern =
    val option: SlashCommandData => SlashCommandData = (data: SlashCommandData) => SlashPatternHelper.addOption[T](data, name, description, autoComplete)
    copy(builderOptions = builderOptions :+ option)

  override def toString: String = s"/$name"
