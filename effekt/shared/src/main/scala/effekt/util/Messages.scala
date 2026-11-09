package effekt
package util

import effekt.source.{Origin, Span, Tree}
import effekt.symbols.{Capture, Captures, Effects, ErrorMessageInterpolator, Name, QualifiedName, TypePrinter}
import kiama.util.Range

object messages {

  type EffektMessages = Vector[Diagnostic]

  enum Severity { case Error, Warning, Info, Hint }

  case class Label(span: Span)

  case class Diagnostic(severity: Severity, primary: Option[Label], subdiagnostics: List[Subdiagnostic], reason: Reason, phase: Option[String]) {
    def range: Option[Range] = primary.map(_.span.range)
  }

  enum Subdiagnostic {
    /** A fact about the diagnostic, possibly at its own place. */
    case Note(text: StructuredMessage, at: Option[Span])
    /** A way to fix the diagnostic. */
    case Help(text: StructuredMessage)

    def text: StructuredMessage
  }

  final class Subdiagnostics private () {
    private[messages] val collected = scala.collection.mutable.ListBuffer.empty[Subdiagnostic]
  }
  object Subdiagnostics {
    /** Handler for Subdiagnostics. */
    def apply(block: Subdiagnostics ?=> Unit): List[Subdiagnostic] = {
      val S = new Subdiagnostics()
      block(using S)
      S.collected.toList
    }
  }

  def help(text: StructuredMessage)(using S: Subdiagnostics): Unit = S.collected += Subdiagnostic.Help(text)
  def note(text: StructuredMessage, at: Option[Span] = None)(using S: Subdiagnostics): Unit = S.collected += Subdiagnostic.Note(text, at)

  /** The exact (open) kind of an issue of a diagnostic. */
  trait Reason {
    def claim: StructuredMessage

    def show(using S: Style): List[String] = List(S.message(claim)) // claim only by default
  }

  /** Semantic style for parts of error messages, leaving the styling to each output. */
  trait Style {
    def title(s: String): String
    def code(s: String): String
    def candidate(s: String): String

    def indent(s: String): String = s.linesIterator.map(l => "  " + l).mkString("\n")

    def content(d: Diagnostic): String = d.reason.show(using this).mkString("\n")

    def subdiagnostic(s: Subdiagnostic): String = message(s.text)

    def message(text: StructuredMessage): String = text.stringContext.s(text.args.map {
      case id: source.IdDef          => code(TypePrinter.show(id))
      case id: source.IdRef          => code(TypePrinter.show(id))
      case name: Name                => code(name.name)
      case t: symbols.Type           => code(TypePrinter.show(t))
      case t: Capture                => code(TypePrinter.show(t))
      case t: Captures               => code(TypePrinter.show(t))
      case t: Effects                => code(TypePrinter.show(t))
      case n: Int                    => code(n.toString)
      case nested: Diagnostic        => contentWithSubdiagnostics(nested)
      case nested: StructuredMessage => message(nested)
      case other                     => other.toString
    }*)

    def contentWithSubdiagnostics(d: Diagnostic): String =
      (content(d) :: d.subdiagnostics.map(subdiagnostic)).mkString("\n")
  }

  case class StructuredMessage(stringContext: StringContext, args: Seq[Any])

  implicit class ErrorMessageReifier(private val sc: StringContext) extends AnyVal {
    def pretty(args: Any*): StructuredMessage = StructuredMessage(sc, args)
  }

  case class Message(text: StructuredMessage) extends Reason {
    def claim = text
  }
  object Message {
    def apply(text: String): Message = Message(pretty"${text}")
  }

  def pluralized(n: Int, singular: String): String =
    if (n == 1) s"$n $singular" else s"$n ${singular}s"

  case class TypeMismatch(expected: symbols.Type, got: symbols.Type) extends Reason {
    def claim = pretty"Expected ${expected} but got ${got}."

    // long types are shown aligned, one above the other
    override def show(using S: Style): List[String] = {
      val expectedShown = TypePrinter.show(expected)
      val gotShown = TypePrinter.show(got)
      if ((expectedShown.size + gotShown.size) < 25) List(S.message(claim)) // TODO: think of a better way to do this...
      else List("Expected type", s"  ${S.code(expectedShown)}", "but got type", s"  ${S.code(gotShown)}")
    }
  }

  private[effekt] def fullname(n: Name): String = n match {
    case n: QualifiedName => n.qualifiedName
    case n: Name          => n.name
  }

  /** Overload resolution that did not resolve: either no candidate fits, or several do. */
  case class OverloadResolution(candidates: List[Candidate]) extends Reason {
    def ambiguous: Boolean = candidates.count { c => c.errors.isEmpty } > 1

    def claim = if ambiguous then pretty"Ambiguous overload." else pretty"Cannot typecheck call."

    override def show(using S: Style): List[String] =
      if ambiguous then {
        val longestNameLength = candidates.map { c => fullname(c.symbol.name).size }.max
        val explanations = candidates.map { c =>
          val name = fullname(c.symbol.name)
          val padding = " " * (longestNameLength - name.size)
          pp"- ${S.code(name)}: ${padding}${c.tpe}"
        }
        S.title(S.message(claim)) :: "There are multiple overloads, which all would type check:" :: explanations
      } else {
        val explanations = candidates.map { c =>
          val nestedErrors = c.errors.map(S.contentWithSubdiagnostics).mkString("\n")
          val header = S.candidate(pp"Possible overload: ${S.code(fullname(c.symbol.name))}") + S.candidate(pp" of type ${c.tpe}")
          s"$header\n${S.indent(nestedErrors)}"
        }
        S.title(S.message(claim)) :: "There are multiple overloads, which all fail to check:" ::
          "" :: explanations.flatMap { e => List(e, "") }
      }
  }

  case class Candidate(symbol: symbols.BlockSymbol, tpe: symbols.FunctionType, errors: EffektMessages)

  // Iterative deepening of the implicit search matches on this class.
  case class ImplicitMaxRecursion(text: String) extends Reason {
    def claim = pretty"${text}"
  }

  case class ImplicitInstantiation(stencil: source.GenerateImplicitArgs.Error, tpe: symbols.Type) extends Reason {
    def claim = pretty"Cannot instantiate implicit argument."

    override def show(using S: Style): List[String] = {
      def formatIndex(i: Int): String = i match {
        case 0 => "1st"
        case 1 => "2nd"
        case 2 => "3rd"
        case n => s"${n + 1}th"
      }
      val prettyIndex = formatIndex(stencil.index)
      val prettyTpe = TypePrinter.show(tpe)
      val mainMessage = S.candidate(s"Cannot instantiate ${prettyIndex} ${stencil.kind} ${stencil.name} of type ${prettyTpe}.")
      val info = stencil.explanation match {
        case Some(e) => S.indent(e) + "\n"
        case None => ""
      }
      val nestedErrors = S.indent(stencil.msgs.map(S.contentWithSubdiagnostics).mkString("\n"))

      List(S.title(S.message(claim)), mainMessage + "\n" + info + nestedErrors + "\n")
    }
  }

  /**
   * Error that aborts a compilation phase
   *
   * Messages are part of the reporting pipeline and can be backtracked by Typer
   */
  case class FatalPhaseError(message: Diagnostic) extends Exception(message.toString)

  /**
   * Error that aborts the whole compilation and shows a stack trace
   *
   * Should be used for unexpected internal compiler errors
   */
  case class CompilerPanic(message: Diagnostic) extends Exception {
    override def toString = message.toString // TODO render!
  }

  def TODO(msg: String = ""): Nothing =
    val explanation = if (msg.isEmpty) "Not implemented, yet" else s"Not implemented, yet: ${msg}"
    throw CompilerPanic(Diagnostic(Severity.Error, None, Nil, Message(explanation), None))

  def FIXME[A](a: A, msg: String = ""): A = a

  def NOT_SUPPORTED(msg: String = ""): Nothing =
    val explanation = if (msg.isEmpty) "Currently not supported" else s"Currently not supported: ${msg}"
    throw CompilerPanic(Diagnostic(Severity.Error, None, Nil, Message(explanation), None))

  def INTERNAL_ERROR(msg: String = ""): Nothing =
    val explanation = if (msg.isEmpty) "Internal compiler error" else s"Internal compiler error: ${msg}"
    throw CompilerPanic(Diagnostic(Severity.Error, None, Nil, Message(explanation), None))

  /**
   * Stores messages in a mutable field
   */
  trait BufferedMessaging {
    private var messages: EffektMessages = Vector.empty

    // TODO filter duplicate messages for overlapping positions. If there are multiple ones, pick the most specific location
    def append(msg: EffektMessages): Unit =
      messages = messages ++ msg

    def append(m: Diagnostic): Unit = messages = messages.appended(m)

    def get: EffektMessages = messages

    def hasErrors: Boolean = messages.exists {
      m => m.severity == Severity.Error
    }

    def clear(): Unit = { messages = Vector.empty }

    def buffer: EffektMessages = messages
    def buffer_=(msgs: EffektMessages): Unit = messages = msgs
  }

  trait EffektMessaging extends BufferedMessaging {

    /**
     * To allow uniform testing on all platforms, we homogenize the paths to Unix-style.
     *
     * This way the negative tests look the same on Windows and Linux
     */
    def homogenizePath(label: String): String =
      label.replace('\\', '/')

    def formatContent(msg: Diagnostic): String

    def formatMessage(msg: Diagnostic): String

    // Filter out duplicates
    // TODO this is a hack and should be solved in typer, where the messages are generated by unification
    def formatMessages(messages: EffektMessages): String =
      messages.sortBy { m => m.range.map(r => (r.from.line, r.from.column)) }.map(formatMessage).distinct.mkString("")

    def report(messages: EffektMessages, emitter: kiama.util.Emitter): Unit =
      emitter.emit(formatMessages(messages))
  }

  class DebugMessaging extends EffektMessaging {
    def formatContent(msg: Diagnostic): String = msg.toString
    def formatMessage(msg: Diagnostic): String = s"${formatContent(msg)}\n"
  }

  /**
   * Every phase has a context that extends ErrorReporter
   */
  trait ErrorReporter {

    var focus: Tree // the current focus of the compiler

    /** The innermost phase currently running, recorded in every diagnostic made while it runs. */
    private var phase: Option[String] = None

    def inPhase[T](name: String)(block: => T): T = {
      val before = phase
      phase = Some(name)
      try block finally phase = before
    }

    val messaging: BufferedMessaging

    def diagnostic(reason: Reason, severity: Severity, at: Option[Span]): Diagnostic =
      // spans with a missing origin give no position
      Diagnostic(severity, at.filter { s => s.origin != Origin.Missing }.map(Label(_)), Nil, reason, phase)

    def diagnostic(reason: Reason, severity: Severity): Diagnostic =
      diagnostic(reason, severity, spanOf(focus))

    def plainMessage(text: String, severity: Severity): Diagnostic =
      diagnostic(Message(text), severity)

    def structuredMessage(content: StructuredMessage, severity: Severity): Diagnostic =
      diagnostic(Message(content), severity)

    def report(msg: Diagnostic): Unit = messaging.append(msg)

    def error(msg: String): Unit = report(plainMessage(msg, Severity.Error))
    def error(msg: StructuredMessage): Unit = report(structuredMessage(msg, Severity.Error))
    def error(msg: StructuredMessage)(subdiagnostics: Subdiagnostics ?=> Unit): Unit =
      report(structuredMessage(msg, Severity.Error).copy(subdiagnostics = Subdiagnostics(subdiagnostics)))
    def error(reason: Reason)(subdiagnostics: Subdiagnostics ?=> Unit): Unit =
      report(diagnostic(reason, Severity.Error).copy(subdiagnostics = Subdiagnostics(subdiagnostics)))

    def panic(msg: Diagnostic): Nothing = throw CompilerPanic(msg)
    def panic(msg: String): Nothing = panic(plainMessage(msg, Severity.Error))
    def panic(msg: StructuredMessage): Nothing = panic(structuredMessage(msg, Severity.Error))

    def warning(msg: String): Unit = report(plainMessage(msg, Severity.Warning))
    def warning(msg: StructuredMessage): Unit = report(structuredMessage(msg, Severity.Warning))
    def warning(msg: StructuredMessage)(subdiagnostics: Subdiagnostics ?=> Unit): Unit =
      report(structuredMessage(msg, Severity.Warning).copy(subdiagnostics = Subdiagnostics(subdiagnostics)))

    def info(msg: String): Unit = report(plainMessage(msg, Severity.Info))
    def info(msg: StructuredMessage): Unit = report(structuredMessage(msg, Severity.Info))

    def abort(msg: Diagnostic): Nothing = throw FatalPhaseError(msg)
    def abort(msg: String): Nothing = abort(plainMessage(msg, Severity.Error))
    def abort(msg: StructuredMessage): Nothing = abort(structuredMessage(msg, Severity.Error))
    def abort(msg: StructuredMessage)(subdiagnostics: Subdiagnostics ?=> Unit): Nothing =
      abort(structuredMessage(msg, Severity.Error).copy(subdiagnostics = Subdiagnostics(subdiagnostics)))
    def abort(reason: Reason): Nothing = abort(diagnostic(reason, Severity.Error))
    def abort(reason: Reason)(subdiagnostics: Subdiagnostics ?=> Unit): Nothing =
      abort(diagnostic(reason, Severity.Error).copy(subdiagnostics = Subdiagnostics(subdiagnostics)))

    def reraise(msg: EffektMessages): Unit = messaging.append(msg)

    // assumes errs is a non empty vector
    def abortWith(errs: EffektMessages): Nothing = {
      val msg = errs.head
      val msgs = errs.tail
      reraise(msgs)
      abort(msg)
    }

    def at[T](t: Tree)(block: => T): T = {
      val before = focus
      focus = t
      try block finally focus = before
    }

    def withFocus[T <: Tree, R](block: T => R): T => R = { t =>
      at(t) { block(t) }
    }

    /**
     * Sets the given tree into focus for error reporting
     */
    def focusing[T <: Tree, R](t: T)(f: T => R): R =
      at(t) { f(t) }

    def spanOf(t: Tree): Option[Span] = Option(t).map(_.span)
  }
}
