package effekt
package util


import effekt.source.Tree

import org.bitbucket.inkytonik.kiama.util.{ Messaging, Message }
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
  }

  /**
   * Every phase has a context that extends ErrorReporter
   */
  trait ErrorReporter {

    def focus: Tree           // the current focus of the compiler
    def buffer: MessageBuffer

    def error(msg: String) = buffer append Messaging.error(focus, msg)
    def warning(msg: String) = buffer append Messaging.warning(focus, msg)
    def info(msg: String) = buffer append Messaging.info(focus, msg)
    def hint(msg: String) = buffer append Messaging.hint(focus, msg)
    def abort(msg: String): Nothing = {
      throw FatalPhaseError(msg, this)
    }

    def aborting[T](block: => T): T =
      try {
        block
      }
      catch {
        // TODO refactor!
        case t if !t.isInstanceOf[FatalPhaseError] =>
          // TODO pass original exception, instead of just the message
          // TODO add a debug flag to show the stack trace
          throw FatalPhaseError(t.getMessage, this)
      }

    // TODO move to a separate Context type, once factored out into a superclass Phase

    /**
     * This is useful to write code like: reporter in { ... implicitly uses reporter ... }
     */
    def in[T](block: (given this.type) => T): T = block(given this)
  }
}