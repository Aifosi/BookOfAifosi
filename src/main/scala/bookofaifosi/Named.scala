package bookofaifosi

trait Named:

  def className: String =
    getClass.getSimpleName.split("\\$").last.replaceAll("([a-z])([A-Z])", "$1 $2")
