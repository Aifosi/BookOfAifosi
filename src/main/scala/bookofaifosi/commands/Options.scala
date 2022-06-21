package bookofaifosi.commands

type PatternOption = SlashPattern => SlashPattern

object Options:
  lazy val duration: PatternOption = _.addOption[Long]("duration", "Amount of time you want to be reminded in.")
  lazy val timeUnit: PatternOption = _.addOption[String]("unit", "Unit of time for the duration", autoComplete = true)
  lazy val tagName: PatternOption = _.addOption[String]("name", "The name of the tag you want to update.", autoComplete = true)

trait Options:
  this: SlashCommand =>
  val options: List[PatternOption]

  override lazy val pattern: SlashPattern = slashPattern.applyOptions(options)
