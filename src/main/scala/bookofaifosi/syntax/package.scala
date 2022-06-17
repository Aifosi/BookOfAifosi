package bookofaifosi

package object syntax:
  object chaining extends ChainingSyntax
  object jda extends JDASyntax
  object action extends ActionSyntax
  object io extends IOSyntax

  object all extends ChainingSyntax with JDASyntax with ActionSyntax with IOSyntax
