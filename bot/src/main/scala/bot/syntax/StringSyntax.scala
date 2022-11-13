package bot.syntax

trait StringSyntax:
  extension (string: String)
    private def charSeparatedToPascal(char: String): String =
      string.split(char).map(_.capitalize).mkString
    private def pascalToCharSeparated(char: String): String =
      string.replaceAll("(?<=[a-z])(?=[A-Z])", char).toLowerCase
      
    def snakeToPascalCase: String = charSeparatedToPascal("_")
    def pascalToSnakeCase: String = pascalToCharSeparated("_")
    
    def kebabToPascalCase: String = charSeparatedToPascal("-")
    def pascalToKebabCase: String = pascalToCharSeparated("-")
