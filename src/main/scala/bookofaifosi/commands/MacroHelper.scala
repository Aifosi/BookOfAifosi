package bookofaifosi.commands

import bookofaifosi.model.{Role, User, Channel}
import net.dv8tion.jda.api.events.interaction.command.{CommandAutoCompleteInteractionEvent, SlashCommandInteractionEvent}
import net.dv8tion.jda.api.interactions.commands.build.{SlashCommandData, SubcommandData}
import net.dv8tion.jda.api.interactions.commands.{OptionMapping, OptionType}
import bookofaifosi.syntax.action.*
import cats.syntax.functor.*
import cats.effect.IO

import java.util
import scala.compiletime.*
import scala.quoted.*
import scala.jdk.CollectionConverters.*

object MacroHelper:

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
      case '[Option[Channel]] => '{ partial(OptionType.CHANNEL, false) }
      //case '[Option[Mentionable]] => '{ partial(OptionType.MENTIONABLE, false) }
      case '[Int] => '{ partial(OptionType.INTEGER, true) }
      case '[Long] => '{ partial(OptionType.INTEGER, true) }
      case '[Double] => '{ partial(OptionType.NUMBER, true) }
      case '[String] => '{ partial(OptionType.STRING, true) }
      case '[Boolean] => '{ partial(OptionType.BOOLEAN, true) }
      case '[User] => '{ partial(OptionType.USER, true) }
      case '[Role] => '{ partial(OptionType.ROLE, true) }
      case '[Channel] => '{ partial(OptionType.CHANNEL, true) }
      //case '[Mentionable] => '{ partial(OptionType.MENTIONABLE, true) }
      case _ => '{ error("Options of given type not supported") }
    }

  inline def addOption[T] = ${ matchT[T] }

  private def subCommandPartial(optionType: OptionType, required: Boolean)(data: SubcommandData, name: String, description: String, autoComplete: Boolean): SubcommandData =
    data.addOption(optionType, name, description, required, autoComplete)

  private def subCommandMatchT[T: Type](using Quotes): Expr[(SubcommandData, String, String, Boolean) => SubcommandData] =
    Type.of[T] match {
      case '[Option[Int]] => '{ subCommandPartial(OptionType.INTEGER, false) }
      case '[Option[Long]] => '{ subCommandPartial(OptionType.INTEGER, false) }
      case '[Option[Double]] => '{ subCommandPartial(OptionType.NUMBER, false) }
      case '[Option[String]] => '{ subCommandPartial(OptionType.STRING, false) }
      case '[Option[Boolean]] => '{ subCommandPartial(OptionType.BOOLEAN, false) }
      case '[Option[User]] => '{ subCommandPartial(OptionType.USER, false) }
      case '[Option[Role]] => '{ subCommandPartial(OptionType.ROLE, false) }
      case '[Option[Channel]] => '{ subCommandPartial(OptionType.CHANNEL, false) }
      //case '[Option[Mentionable]] => '{ subCommandPartial(OptionType.MENTIONABLE, false) }
      case '[Int] => '{ subCommandPartial(OptionType.INTEGER, true) }
      case '[Long] => '{ subCommandPartial(OptionType.INTEGER, true) }
      case '[Double] => '{ subCommandPartial(OptionType.NUMBER, true) }
      case '[String] => '{ subCommandPartial(OptionType.STRING, true) }
      case '[Boolean] => '{ subCommandPartial(OptionType.BOOLEAN, true) }
      case '[User] => '{ subCommandPartial(OptionType.USER, true) }
      case '[Role] => '{ subCommandPartial(OptionType.ROLE, true) }
      case '[Channel] => '{ subCommandPartial(OptionType.CHANNEL, true) }
      //case '[Mentionable] => '{ subCommandPartial(OptionType.MENTIONABLE, true) }
      case _ => '{ error("Options of given type not supported") }
    }

  inline def addSubCommandOption[T] = ${ subCommandMatchT[T] }


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
      case '[Option[Channel]] => '{ getOptionT[Channel](mapping => new Channel(mapping.getAsMessageChannel))(_, _).asInstanceOf[T] }
      //case '[Option[Mentionable]] => '{ getOptionT[Mentionable](_.getAsMentionable)(_, _).asInstanceOf[T] }
      case '[Int] => '{ getT[Int](_.getAsLong.toInt)(_, _).asInstanceOf[T] }
      case '[Long] => '{ getT[Long](_.getAsLong)(_, _).asInstanceOf[T] }
      case '[Double] => '{ getT[Double](_.getAsDouble)(_, _).asInstanceOf[T] }
      case '[String] => '{ getT[String](_.getAsString)(_, _).asInstanceOf[T] }
      case '[Boolean] => '{ getT[Boolean](_.getAsBoolean)(_, _).asInstanceOf[T] }
      case '[User] => '{ getT[User](mapping => new User(mapping.getAsUser))(_, _).asInstanceOf[T] }
      case '[Role] => '{ getT[Role](mapping => new Role(mapping.getAsRole))(_, _).asInstanceOf[T] }
      case '[Channel] => '{ getT[Channel](mapping => new Channel(mapping.getAsMessageChannel))(_, _).asInstanceOf[T] }
      //case '[Mentionable] => '{ getT[Mentionable](_.getAsMentionable)(_, _).asInstanceOf[T] }
      case _ => '{ error("Options of given type not supported") }
    }

  inline def getOption[T] = ${ fetchOption[T] }

  private def replyOptions[T: Type](using Quotes): Expr[(CommandAutoCompleteInteractionEvent, List[T]) => IO[Unit]] =
    Type.of[T] match {
      /*case '[Option[Int]] => '{ (event: CommandAutoCompleteInteractionEvent, options: List[T]) => event.replyChoiceLongs(options.asJava).toIO.void }
      case '[Option[Long]] => '{ (event: CommandAutoCompleteInteractionEvent, options: List[T]) => event.replyChoiceLongs(options.asJava).toIO.void }
      case '[Option[Double]] => '{ (event: CommandAutoCompleteInteractionEvent, options: List[T]) => event.replyChoiceDoubles(options.asJava).toIO.void }
      case '[Option[String]] => '{ (event: CommandAutoCompleteInteractionEvent, options: List[T]) => event.replyChoiceStrings(options.asJava).toIO.void }*/
      //case '[Int] => '{ makeReplyInt(_, _) }
      case '[Int] => '{ (event: CommandAutoCompleteInteractionEvent, options: List[T]) => event.replyChoiceLongs(options.asInstanceOf[List[Int]].map(_.toLong)*).toIO.void }
      case '[Long] => '{ (event: CommandAutoCompleteInteractionEvent, options: List[T]) => event.replyChoiceLongs(options.asInstanceOf[List[Long]]*).toIO.void }
      case '[Double] => '{ (event: CommandAutoCompleteInteractionEvent, options: List[T]) => event.replyChoiceDoubles(options.asInstanceOf[List[Double]]*).toIO.void }
      case '[String] => '{ (event: CommandAutoCompleteInteractionEvent, options: List[T]) => event.replyChoiceStrings(options.asInstanceOf[List[String]]*).toIO.void }
      case _ => '{ error("Options of given type not supported") }
    }

  inline def replyChoices[T] = ${ replyOptions[T] }
