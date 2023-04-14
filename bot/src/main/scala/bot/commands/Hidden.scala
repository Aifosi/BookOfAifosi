package bot.commands

trait Hidden:
  this: AnyCommand =>
  //Description is used to auto generate a help command and hidden commands are now shown there
  override final val description: String = ""
