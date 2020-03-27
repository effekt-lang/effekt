package effekt
package util

import effekt.source.Tree

import org.bitbucket.inkytonik.kiama.util.{ Messaging }
import org.bitbucket.inkytonik.kiama.util.Messaging.{ Messages, noMessages }
import org.bitbucket.inkytonik.kiama.util.Severities.Error

object messages {

  case class FatalPhaseError(msg: String, reporter: ErrorReporter) extends Exception

  /**
   * Stores messages in a mutable field
   */
  class MessageBuffer {
    private var messages: Messages = noMessages
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

    def error(msg: String): Unit = buffer append Messaging.error(focus, msg)
    def warning(msg: String): Unit = buffer append Messaging.warning(focus, msg)
    def info(msg: String): Unit = buffer append Messaging.info(focus, msg)
    def hint(msg: String): Unit = buffer append Messaging.hint(focus, msg)
    def abort(msg: String): Nothing = {
      throw FatalPhaseError(msg, this)
    }

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
