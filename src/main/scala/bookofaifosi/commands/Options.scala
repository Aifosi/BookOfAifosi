package bookofaifosi.commands

type PatternOption = SlashPattern => SlashPattern

object Options:
  def duration(option: String): PatternOption = _.addOption[Long](option, "Amount of time you.")
  lazy val timeUnit: PatternOption = _.addOption[String]("unit", "Unit of time.", autoComplete = true)
  lazy val tagName: PatternOption = _.addOption[String]("name", "The name of the tag you want to update.", autoComplete = true)
  lazy val lockName: PatternOption = _.addOption[String]("lock", "Name of the lock.", autoComplete = true)

trait Options:
  this: SlashCommand =>
  val options: List[PatternOption]

  override lazy val pattern: SlashPattern = slashPattern.applyOptions(options)
