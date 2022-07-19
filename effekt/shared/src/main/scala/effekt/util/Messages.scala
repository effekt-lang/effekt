package effekt
package util

import effekt.source.{ NoSource, Tree }
import kiama.util.{ Messages, noMessages, Messaging, Positions, Range, message }
import kiama.util.Severities.*

object messages {

  /**
   * Error that aborts a compilation phase
   *
   * Messages are part of the reporting pipeline and can be backtracked by Typer
   */
  case class FatalPhaseError(range: Option[Range], msg: String) extends Exception

  /**
   * Error that aborts the whole compilation and shows a stack trace
   *
   * Should be used for unexpected internal compiler errors
   */
  case class CompilerPanic(msg: String, position: Option[Range]) extends Exception {
    override def toString = msg
  }

  /**
   * Stores messages in a mutable field
   */
  class MessageBuffer {
    private var messages: Messages = noMessages

    // TODO filter duplicate messages for overlapping positions. If there are multiple ones, pick the most specific location
    def append(msg: Messages): Unit = {
      messages = messages ++ msg
    }
    def get: Messages = messages

    def hasErrors: Boolean = messages.exists {
      m => m.severity == Error
    }

    def clear(): Unit = { messages = noMessages }
  }

  /**
   * Every phase has a context that extends ErrorReporter
   */
  trait ErrorReporter {

    var focus: Tree // the current focus of the compiler
    def buffer: MessageBuffer

    def positions: Positions // used to lookup positions of trees

    def error(at: Option[Range], msg: String): Unit = buffer append message(at, msg, Error)
    def error(at: Tree, msg: String): Unit = error(rangeOf(at), msg)
    def error(msg: String): Unit = error(focus, msg)

    def panic(at: Option[Range], msg: String): Nothing = throw CompilerPanic(msg, at)
    def panic(at: Tree, msg: String): Nothing = panic(rangeOf(at), msg)
    def panic(msg: String): Nothing = panic(focus, msg)

    def warning(at: Option[Range], msg: String): Unit = buffer append message(at, msg, Warning)
    def warning(msg: String): Unit = warning(focus, msg)
    def warning(at: Tree, msg: String): Unit = warning(rangeOf(at), msg)

    def info(at: Option[Range], msg: String): Unit = buffer append message(at, msg, Information)
    def info(at: Tree, msg: String): Unit = info(rangeOf(at), msg)
    def info(msg: String): Unit = info(focus, msg)

    def abort(at: Option[Range], msg: String): Nothing = throw FatalPhaseError(at, msg)
    def abort(at: Tree, msg: String): Nothing = abort(rangeOf(at), msg)
    def abort(msg: String): Nothing = abort(focus, msg)

    def reraise(msg: Messages): Unit = buffer.append(msg)

    def at[T](t: Tree)(block: => T): T = {
      val before = focus
      focus = t;
      val res = block;
      focus = before;
      res
    }

    def withFocus[T <: Tree, R](block: T => R): T => R = { t =>
      at(t) { block(t) }
    }

    /**
     * Sets the given tree into focus for error reporting
     */
    def focusing[T <: Tree, R](t: T)(f: T => R): R =
      at(t) { f(t) }

    private def rangeOf(t: Tree): Option[Range] = positions.getRange(t)
  }
}
