package effekt
package util

import effekt.util.messages.*
import kiama.util.{ Position, Range }

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
    val formatted = formatOne(message, contentWithPhase(message))
    val items = message.subdiagnostics.map(item)
    // one gutter for the whole diagnostic, as wide as its largest line number
    val width = (formatted.excerpt ++ items.flatMap(_.excerpt)).map { e => e.line.toString.length }.maxOption.getOrElse(0)
    // a blank line between the diagnostic and its items, and between items that span several lines
    val rendered = items.zipWithIndex.map { case (it, i) =>
      val gap = i == 0 || it.excerpt.nonEmpty || items(i - 1).excerpt.nonEmpty
      (if gap then "\n" else "") + it.render(width)
    }
    formatted.render(width) + rendered.mkString("")
  }

  case class Excerpt(line: Int, code: String, marker: String)

  case class Formatted(header: String, excerpt: Option[Excerpt]) {
    def render(width: Int): String = excerpt match {
      case Some(Excerpt(line, code, marker)) =>
        s"${header}\n${dim(" " * (width - line.toString.length) + line)}  ${code}\n${" " * width}  ${marker}\n"
      case None => s"${header}\n"
    }
  }

  // a note or a help, under the diagnostic it belongs to
  case class Item(kind: String, text: String, excerpt: Option[Excerpt]) {
    def render(width: Int): String = {
      val column = " " * (width + 2)
      s"${column}* ${kind}: ${text}\n" + (excerpt match {
        case Some(Excerpt(line, code, marker)) =>
          s"${dim(" " * (width - line.toString.length) + line)}  ${dim("|")}  ${code}\n${column}${dim("|")}  ${marker}\n"
        case None => ""
      })
    }
  }

  private def item(s: Subdiagnostic): Item = s match {
    case Subdiagnostic.Note(text, at) => Item("note", message(text), at.flatMap { span => excerpt(span.range) })
    case Subdiagnostic.Help(text)     => Item("help", message(text), None)
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

  private def formatOne(message: Diagnostic, text: String): Formatted = {
    val severity = severityToWord(message.severity)
    message.range match {
      case Some(range) => Formatted(s"[$severity] ${homogenizePath(place(range.from))} ${text}", excerpt(range))
      case None        => Formatted(s"[$severity] ${text}", None)
    }
  }

  // the line where `range` starts, marked to its end on that line
  def excerpt(range: Range): Option[Excerpt] = {
    val from = range.from
    val to = if (range.from.line == range.to.line) range.to else from.copy(column = from.column + 1)
    from.source.optLineContents(from.line).map { src =>
      Excerpt(from.line, util.AnsiHighlight(src), (" " * (from.column - 1)) + ("^" * (to.column - from.column)))
    }
  }

  override def formatContent(err: Diagnostic): String = content(err)

  def place(p: Position): String = {
    val name = shownName(p.source)
    if (name == "") s"${p.line}:${p.column}:" else s"${name}:${p.line}:${p.column}:"
  }
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
  override def excerpt(range: Range): Option[Excerpt] = None
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

