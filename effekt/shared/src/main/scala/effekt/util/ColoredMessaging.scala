package effekt
package util

import effekt.util.messages.*
import kiama.util.Position

trait ColoredMessaging extends EffektMessaging with Style {

  // Colors
  // ------

  def yellow(s: String): String
  def red(s: String): String
  def white(s: String): String
  def bold(s: String): String
  def highlight(s: String): String
  def underlined(s: String): String

  def title(s: String): String = bold(s)
  def code(s: String): String = highlight(s)
  def candidate(s: String): String = underlined(s)

  def severityToWord(severity: Severity): String =
    severity match {
      case Severity.Error   => red("error")
      case Severity.Warning => yellow("warning")
      case Severity.Info    => white("info")
      case Severity.Hint    => "hint"
    }

  override def formatMessage(message: Diagnostic): String =
    (message :: message.subdiagnostics.map { s => asInfo(message, s) }).map(formatOne).mkString("")

  private def asInfo(d: Diagnostic, s: Subdiagnostic): Diagnostic = {
    val at = s match {
      case Subdiagnostic.Note(_, Some(span)) => Some(Label(span))
      case _ => d.primary
    }
    Diagnostic(Severity.Info, at, Nil, Message(s.text), d.phase)
  }

  private def formatOne(message: Diagnostic): String =
    message.range match {
      case Some(range) if range.from.line == range.to.line => formatMessage(message, range.from, range.to)
      case Some(range) => formatMessage(message, range.from)
      case None =>
        val severity = severityToWord(message.severity)
        s"[$severity] ${formatContent(message)}\n"
    }

  def formatMessage(message: Diagnostic, from: Position): String =
    formatMessage(message, from, from.copy(column = from.column + 1))

  def formatMessage(message: Diagnostic, from: Position, to: Position): String = {
    val severity = severityToWord(message.severity)
    val context = from.source.optLineContents(from.line).map { src =>
      util.AnsiHighlight(src) + "\n" + (" " * (from.column - 1)) + ("^" * (to.column - from.column))
    }.getOrElse("")
    s"[$severity] ${homogenizePath(from.format)} ${formatContent(message)}\n$context\n"
  }

  override def formatContent(err: Diagnostic): String = content(err)
}

class PlainMessaging extends ColoredMessaging {
  def yellow(s: String): String = s
  def red(s: String): String = s
  def white(s: String): String = s
  def bold(s: String): String = s
  def highlight(s: String): String = s
  def underlined(s: String): String = s

  // Don't show context in plain messaging.
  override def formatMessage(message: Diagnostic, from: Position): String = {
    val severity = severityToWord(message.severity)
    s"[$severity] ${homogenizePath(from.format)} ${formatContent(message)}\n"
  }

  override def formatMessage(message: Diagnostic, from: Position, to: Position): String =
    formatMessage(message, from)
}

class AnsiColoredMessaging extends ColoredMessaging {
  def yellow(s: String): String = Console.YELLOW + s + Console.RESET
  def red(s: String): String = Console.RED + s + Console.RESET
  def white(s: String): String = Console.WHITE + s + Console.RESET
  def bold(s: String): String = Console.BOLD + s + Console.RESET
  def highlight(s: String): String = Console.BLACK + Console.WHITE_B + s + Console.RESET
  def underlined(s: String): String = Console.UNDERLINED + s + Console.RESET
}

