package effekt
package context

import util.{ PlainMessaging, Timers }
import util.messages.{ EffektMessages, ErrorReporter }

trait Context extends ErrorReporter, Timers {
  // cache used by tasks to save their results (in addition to information in the AnnotationsDB)
  var cache: util.Task.Cache = util.Task.emptyCache
  val messaging = new PlainMessaging

  // temporarily switches the message buffer to collect messages
  def withMessages[T](block: => T): (EffektMessages, T) = {
    val bufferBefore = messaging.buffer

    messaging.clear()

    val res = block
    val msgs = messaging.buffer
    messaging.buffer = bufferBefore
    (msgs, res)
  }

  object config {
    def maxInlineSize() = 30L
    def optimize() = true
  }
}

/**
 * Helper method to find the currently implicit context
 */
def Context(using C: Context): C.type = C
