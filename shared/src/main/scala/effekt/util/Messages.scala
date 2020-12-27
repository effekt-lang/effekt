package effekt
package util

import effekt.source.Tree

import org.bitbucket.inkytonik.kiama.util.{ Messaging, Message }
import org.bitbucket.inkytonik.kiama.util.Messaging.{ Messages, noMessages }
import org.bitbucket.inkytonik.kiama.util.Severities.Error

object messages {

  /**
   * Error that aborts a compilation phase
   *
   * Messages are part of the reporting pipeline and can be backtracked by Typer
   */
  case class FatalPhaseError(msg: String) extends Exception

  /**
   * Error that aborts the whole compilation and shows a stack trace
   *
   * Should be used for unexpected internal compiler errors
   */
  case class CompilerPanic(msg: String, position: AnyRef) extends Exception

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

    def error(msg: String): Unit = error(focus, msg)
    def error(at: AnyRef, msg: String): Unit = buffer append Messaging.error(at, msg)

    def panic(msg: String): Nothing = panic(focus, msg)
    def panic(at: AnyRef, msg: String): Nothing = throw CompilerPanic(msg, at)

    def warning(msg: String): Unit = warning(focus, msg)
    def warning(at: AnyRef, msg: String): Unit = buffer append Messaging.warning(at, msg)

    def info(msg: String): Unit = info(focus, msg)
    def info(at: AnyRef, msg: String): Unit = buffer append Messaging.info(at, msg)

    def abort(msg: String): Nothing = throw FatalPhaseError(msg)

    def reraise(msg: Messages): Unit = buffer.append(msg)

    def at[T](t: Tree)(block: => T): T = {
      val before = focus
      focus = t;
      val res = block;
      focus = before;
      res
    }

    /**
     * Sets the given tree into focus for error reporting
     */
    def focusing[T <: Tree, R](t: T)(f: T => R): R =
      at(t) { f(t) }
  }
}
