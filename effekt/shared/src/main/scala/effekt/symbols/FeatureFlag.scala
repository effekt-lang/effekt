package effekt.symbols

enum FeatureFlag {
  case NamedFeatureFlag(id: String)
  case Default

  def matches(name: String): Boolean = this match {
    case NamedFeatureFlag(n) if n == name => true
    case Default => true
    case _ => false
  }
  def matches(names: List[String]): Boolean = this match {
    case NamedFeatureFlag(n) if names.contains(n) => true
    case Default => true
    case _ => false
  }
}
object FeatureFlag {
  extension[A](self: List[(FeatureFlag, A)]) {
    def forFeatureFlag(name: String): Option[A] = {
      self.collectFirst{
        case (flag, a) if flag.matches(name) => a
      }
    }
    def forFeatureFlags(names: List[String]): Option[A] = names match {
      case Nil => None
      case name :: other =>
        self.collectFirst {
          case (flag, a) if flag.matches(name) => a
        } orElse (self.forFeatureFlags(other))
    }
  }
}
