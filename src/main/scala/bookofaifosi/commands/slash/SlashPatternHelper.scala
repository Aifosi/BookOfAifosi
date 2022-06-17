package bookofaifosi.commands.slash

import net.dv8tion.jda.api.events.interaction.command.SlashCommandInteractionEvent
import net.dv8tion.jda.api.interactions.commands.build.SlashCommandData
import net.dv8tion.jda.api.interactions.commands.{OptionMapping, OptionType}
import bookofaifosi.wrappers.{Role, User}

import scala.compiletime.*
import scala.quoted.*

object SlashPatternHelper:

  private def partial(optionType: OptionType, required: Boolean)(data: SlashCommandData, name: String, description: String, autoComplete: Boolean): SlashCommandData =
    data.addOption(optionType, name, description, required, autoComplete)

  private def matchT[T: Type](using Quotes): Expr[(SlashCommandData, String, String, Boolean) => SlashCommandData] =
    Type.of[T] match {
      case '[Option[Int]] => '{ partial(OptionType.INTEGER, false) }
      case '[Option[Long]] => '{ partial(OptionType.INTEGER, false) }
      case '[Option[Double]] => '{ partial(OptionType.NUMBER, false) }
      case '[Option[String]] => '{ partial(OptionType.STRING, false) }
      case '[Option[Boolean]] => '{ partial(OptionType.BOOLEAN, false) }
      case '[Option[User]] => '{ partial(OptionType.USER, false) }
      case '[Option[Role]] => '{ partial(OptionType.ROLE, false) }
      //case '[Option[Mentionable]] => '{ partial(OptionType.MENTIONABLE, false) }
      case '[Int] => '{ partial(OptionType.INTEGER, true) }
      case '[Long] => '{ partial(OptionType.INTEGER, true) }
      case '[Double] => '{ partial(OptionType.NUMBER, true) }
      case '[String] => '{ partial(OptionType.STRING, true) }
      case '[Boolean] => '{ partial(OptionType.BOOLEAN, true) }
      case '[User] => '{ partial(OptionType.USER, true) }
      case '[Role] => '{ partial(OptionType.ROLE, true) }
      //case '[Mentionable] => '{ partial(OptionType.MENTIONABLE, true) }
      case _ => '{ error("Options of given type not supported") }
    }

  inline def addOption[T] = ${ matchT[T] }


  private def getT[T](f: OptionMapping => T)(event: SlashCommandInteractionEvent, option: String): T =
    f(event.getOption(option))
  private def getOptionT[T](f: OptionMapping => T)(event: SlashCommandInteractionEvent, option: String): Option[T] =
    Option(event.getOption(option)).map(f)

  private def fetchOption[T: Type](using Quotes): Expr[(SlashCommandInteractionEvent, String) => T] =
    //val a: (SlashCommandInteractionEvent, String) => User = getT[User](_.getAsUser)
    Type.of[T] match {
      case '[Option[Int]] => '{ getOptionT[Int](_.getAsLong.toInt)(_, _).asInstanceOf[T] }
      case '[Option[Long]] => '{ getOptionT[Long](_.getAsLong)(_, _).asInstanceOf[T] }
      case '[Option[Double]] => '{ getOptionT[Double](_.getAsDouble)(_, _).asInstanceOf[T] }
      case '[Option[String]] => '{ getOptionT[String](_.getAsString)(_, _).asInstanceOf[T] }
      case '[Option[Boolean]] => '{ getOptionT[Boolean](_.getAsBoolean)(_, _).asInstanceOf[T] }
      case '[Option[User]] => '{ getOptionT[User](mapping => new User(mapping.getAsUser))(_, _).asInstanceOf[T] }
      case '[Option[Role]] => '{ getOptionT[Role](mapping => new Role(mapping.getAsRole))(_, _).asInstanceOf[T] }
      //case '[Option[Mentionable]] => '{ getOptionT[Mentionable](_.getAsMentionable)(_, _).asInstanceOf[T] }
      case '[Int] => '{ getT[Int](_.getAsLong.toInt)(_, _).asInstanceOf[T] }
      case '[Long] => '{ getT[Long](_.getAsLong)(_, _).asInstanceOf[T] }
      case '[Double] => '{ getT[Double](_.getAsDouble)(_, _).asInstanceOf[T] }
      case '[String] => '{ getT[String](_.getAsString)(_, _).asInstanceOf[T] }
      case '[Boolean] => '{ getT[Boolean](_.getAsBoolean)(_, _).asInstanceOf[T] }
      case '[User] => '{ getT[User](mapping => new User(mapping.getAsUser))(_, _).asInstanceOf[T] }
      case '[Role] => '{ getT[Role](mapping => new Role(mapping.getAsRole))(_, _).asInstanceOf[T] }
      //case '[Mentionable] => '{ getT[Mentionable](_.getAsMentionable)(_, _).asInstanceOf[T] }
      case _ => '{ error("Options of given type not supported") }
    }

  inline def getOption[T] = ${ fetchOption[T] }