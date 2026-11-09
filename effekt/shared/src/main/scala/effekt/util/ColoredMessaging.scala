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
  def dim(s: String): String

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

  override def formatMessage(message: Diagnostic): String = {
    val formatted = formatOne(message, contentWithPhase(message)) :: message.subdiagnostics.map { s =>
      val info = asInfo(message, s)
      formatOne(info, formatContent(info))
    }
    // one gutter for the whole diagnostic, as wide as its largest line number
    val width = formatted.flatMap(_.excerpt).map { e => e.line.toString.length }.maxOption.getOrElse(0)
    formatted.map { f => f.render(width) }.mkString("")
  }

  case class Excerpt(line: Int, code: String, marker: String)

  case class Formatted(header: String, excerpt: Option[Excerpt]) {
    def render(width: Int): String = excerpt match {
      case Some(Excerpt(line, code, marker)) =>
        s"${header}\n${dim(" " * (width - line.toString.length) + line)}  ${code}\n${" " * width}  ${marker}\n"
      case None => s"${header}\n"
    }
  }

  private def asInfo(d: Diagnostic, s: Subdiagnostic): Diagnostic = {
    val at = s match {
      case Subdiagnostic.Note(_, Some(span)) => Some(Label(span))
      case _ => d.primary
    }
    Diagnostic(Severity.Info, at, Nil, Message(s.text), d.phase)
  }

  def contentWithPhase(d: Diagnostic): String = {
    val phase = d.phase.map { p => s" ${dim(s"[${p}]")}" }.getOrElse("")
    d.reason.show(using this) match {
      case first :: rest => (first + phase :: rest).mkString("\n")
      case Nil => phase
    }
  }

  def lspSource(d: Diagnostic): String = d.phase match {
    case Some(phase) => s"effekt [${phase}]"
    case None => "effekt"
  }

  private def formatOne(message: Diagnostic, text: String): Formatted =
    message.range match {
      case Some(range) if range.from.line == range.to.line => formatMessage(message, text, range.from, range.to)
      case Some(range) => formatMessage(message, text, range.from)
      case None =>
        val severity = severityToWord(message.severity)
        Formatted(s"[$severity] ${text}", None)
    }

  def formatMessage(message: Diagnostic, text: String, from: Position): Formatted =
    formatMessage(message, text, from, from.copy(column = from.column + 1))

  def formatMessage(message: Diagnostic, text: String, from: Position, to: Position): Formatted = {
    val severity = severityToWord(message.severity)
    val excerpt = from.source.optLineContents(from.line).map { src =>
      Excerpt(from.line, util.AnsiHighlight(src), (" " * (from.column - 1)) + ("^" * (to.column - from.column)))
    }
    Formatted(s"[$severity] ${homogenizePath(from.format)} ${text}", excerpt)
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
  def dim(s: String): String = s

  // Don't show context in plain messaging.
  override def formatMessage(message: Diagnostic, text: String, from: Position): Formatted = {
    val severity = severityToWord(message.severity)
    Formatted(s"[$severity] ${homogenizePath(from.format)} ${text}", None)
  }

  override def formatMessage(message: Diagnostic, text: String, from: Position, to: Position): Formatted =
    formatMessage(message, text, from)
}

class AnsiColoredMessaging extends ColoredMessaging {
  def yellow(s: String): String = Console.YELLOW + s + Console.RESET
  def red(s: String): String = Console.RED + s + Console.RESET
  def white(s: String): String = Console.WHITE + s + Console.RESET
  def bold(s: String): String = Console.BOLD + s + Console.RESET
  def highlight(s: String): String = Console.BLACK + Console.WHITE_B + s + Console.RESET
  def underlined(s: String): String = Console.UNDERLINED + s + Console.RESET
  def dim(s: String): String = "\u001b[2m" + s + Console.RESET
}

