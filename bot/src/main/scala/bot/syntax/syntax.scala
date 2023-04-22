package bot.syntax

object chaining extends ChainingSyntax
object action   extends ActionSyntax
object task     extends TaskSyntax
object io       extends IOSyntax
object kleisli  extends KleisliSyntax
object resource extends ResourceSyntax
object stream   extends StreamSyntax

object all
    extends ChainingSyntax with ActionSyntax with TaskSyntax with IOSyntax with KleisliSyntax with ResourceSyntax
    with StreamSyntax
