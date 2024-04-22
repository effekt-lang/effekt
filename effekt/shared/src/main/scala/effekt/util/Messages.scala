package effekt
package util

import effekt.source.{ Tree }
import kiama.util.{ Message, Messaging, Positions, Range, Severities }
import kiama.util.Severities.*

object messages {

  type EffektMessages = Vector[EffektError]

  sealed trait EffektError extends Message

  /**
   * Every phase has a context that extends ErrorReporter
   */
  trait ErrorReporter {

    var focus: Tree // the current focus of the compiler

    def currentRange: Option[Range] = rangeOf(focus)


    def positions: Positions // used to lookup positions of trees


    def report(msg: EffektError): Unit = ???

    def reraise(msg: EffektMessages): Unit = ()


    def at[T](t: Tree)(block: => T): T = ???

    def withFocus[T <: Tree, R](block: T => R): T => R = ???

    /**
     * Sets the given tree into focus for error reporting
     */
    def focusing[T <: Tree, R](t: T)(f: T => R): R = ???

    def rangeOf(t: Tree): Option[Range] = ???
  }
}
