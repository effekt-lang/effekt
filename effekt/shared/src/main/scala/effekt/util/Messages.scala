package effekt
package util

import effekt.source.{ NoSource, Tree }
import kiama.util.{ Message, Messaging, Positions, Range, Severities }
import kiama.util.Severities.*

object messages {

  type EffektMessages = Vector[EffektError]

  sealed trait EffektError extends Message
  case class ParseError(message: String, range: Option[Range]) extends EffektError { val severity = Error }
  case class AmbiguousOverloadError(matches: List[(symbols.BlockSymbol, symbols.FunctionType)], range: Option[Range]) extends EffektError { val severity = Error }
  case class FailedOverloadError(failedAttempts: List[(symbols.BlockSymbol, symbols.FunctionType, EffektMessages)], range: Option[Range]) extends EffektError { val severity = Error }
  case class PlainTextError(content: String, range: Option[Range], severity: Severity) extends EffektError
  case class StructuredError(content: StructuredMessage, range: Option[Range], severity: Severity) extends EffektError

  case class StructuredMessage(stringContext: StringContext, args: Seq[Any])

  implicit class ErrorMessageReifier(private val sc: StringContext) extends AnyVal {
    def pretty(args: Any*): StructuredMessage = StructuredMessage(sc, args)
  }

  /**
   * Error that aborts a compilation phase
   *
   * Messages are part of the reporting pipeline and can be backtracked by Typer
   */
  case class FatalPhaseError(message: EffektError) extends Exception

  /**
   * Error that aborts the whole compilation and shows a stack trace
   *
   * Should be used for unexpected internal compiler errors
   */
  case class CompilerPanic(message: EffektError) extends Exception {
    override def toString = message.toString // TODO render!
  }

  /**
   * Stores messages in a mutable field
   */
  trait BufferedMessaging[M <: Message] extends Messaging[M] {
    private var messages: Messages = noMessages

    // TODO filter duplicate messages for overlapping positions. If there are multiple ones, pick the most specific location
    def append(msg: Messages): Unit =
      messages = messages ++ msg

    def append(m: M): Unit = messages = messages.appended(m)

    def get: Messages = messages

    def hasErrors: Boolean = messages.exists {
      m => m.severity == Error
    }

    def clear(): Unit = { messages = noMessages }

    def buffer: Messages = messages
    def buffer_=(msgs: Messages): Unit = messages = msgs
  }

  trait EffektMessaging extends BufferedMessaging[EffektError] {

    /**
     * To allow uniform testing on all platforms, we homogenize the paths to Unix-style.
     *
     * This way the negative tests look the same on Windows and Linux
     */
    def homogenizePath(label: String): String =
      label.replace('\\', '/')

    def message(range: Option[Range], content: String, severity: Severity): EffektError = PlainTextError(content, range, severity)
  }

  class DebugMessaging extends EffektMessaging {
    def formatContent(msg: EffektError): String = msg.toString
  }

  /**
   * Every phase has a context that extends ErrorReporter
   */
  trait ErrorReporter {

    var focus: Tree // the current focus of the compiler

    def currentRange: Option[Range] = rangeOf(focus)

    val messaging: BufferedMessaging[EffektError]

    def positions: Positions // used to lookup positions of trees

    def plainMessage(text: String, severity: Severity): EffektError =
      PlainTextError(text, rangeOf(focus), severity)

    def structuredMessage(content: StructuredMessage, severity: Severity): EffektError =
      StructuredError(content, rangeOf(focus), severity)

    def report(msg: EffektError): Unit = messaging.append(msg)

    def error(msg: String): Unit = report(plainMessage(msg, Error))
    def error(msg: StructuredMessage): Unit = report(structuredMessage(msg, Error))

    def panic(msg: EffektError): Nothing = throw CompilerPanic(msg)
    def panic(msg: String): Nothing = panic(plainMessage(msg, Error))
    def panic(msg: StructuredMessage): Nothing = abort(structuredMessage(msg, Error))

    def warning(msg: String): Unit = report(plainMessage(msg, Warning))
    def warning(msg: StructuredMessage): Unit = report(structuredMessage(msg, Warning))

    def info(msg: String): Unit = report(plainMessage(msg, Information))
    def info(msg: StructuredMessage): Unit = report(structuredMessage(msg, Information))

    def abort(msg: EffektError): Nothing = throw FatalPhaseError(msg)
    def abort(msg: String): Nothing = abort(plainMessage(msg, Error))
    def abort(msg: StructuredMessage): Nothing = abort(structuredMessage(msg, Error))

    def reraise(msg: EffektMessages): Unit = messaging.append(msg)

    // assumes errs is a non empty vector
    def abortWith(errs: messaging.Messages): Nothing = {
      val msg = errs.head
      val msgs = errs.tail
      reraise(msgs)
      abort(msg)
    }

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

    def rangeOf(t: Tree): Option[Range] = positions.getRange(t)
  }
}
