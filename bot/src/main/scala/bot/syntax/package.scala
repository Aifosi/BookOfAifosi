package bot

package object syntax:
  object chaining extends ChainingSyntax
  object action extends ActionSyntax
  object task extends TaskSyntax
  object io extends IOSyntax
  object stream extends StreamSyntax
  object string extends StringSyntax

  object all
    extends ChainingSyntax
      with ActionSyntax
      with TaskSyntax
      with IOSyntax
      with StreamSyntax
      with StringSyntax
