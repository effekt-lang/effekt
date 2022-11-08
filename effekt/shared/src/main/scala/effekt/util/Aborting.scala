package effekt.util

extension [T] (self: Option[T]) {
  /**
   * This is basically Scala's [[Option.getOrElse()]] but marked as inline, so we
   * can use non-local returns within the default clause.
   */
  inline def getOrElseAborting(inline default: => T): T = if (self.isDefined) self.get else default

  /**
   * This is basically Scala's [[Option.foreach()]] but marked as inline, so we
   * can use non-local returns within the default clause.
   */
  inline def foreachAborting(inline f: T => Unit): Unit = if (self.nonEmpty) f(self.get)
}

extension [A] (self: List[A]) {

  /**
   * This is basically Scala's [[List.foreach()]] but marked as inline, so we
   * can use non-local returns within the default clause.
   */
  inline def foreachAborting[U](inline f: A => U): Unit = {
    var these = self
    while (these.nonEmpty) {
      f(these.head)
      these = these.tail
    }
  }
}