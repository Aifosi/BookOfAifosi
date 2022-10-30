package bot.syntax

trait ChainingSyntax:
  extension [A](self: A)
    def when(cond: Boolean)(f: A => A): A = if cond then f(self) else self
