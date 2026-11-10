package effekt
package util

import effekt.util.messages.*
import kiama.util.{ Position, Range }

trait ColoredMessaging extends EffektMessaging with Style {

  private def dim(s: String): String = styled(Role.Dim, s)

  def severityToWord(severity: Severity): String = styled(Role.Severity(severity), severity match {
    case Severity.Error   => "error"
    case Severity.Warning => "warning"
    case Severity.Info    => "info"
    case Severity.Hint    => "hint"
  })

  override def formatMessage(message: Diagnostic): String = {
    val formatted = formatOne(message, contentWithPhase(message))
    val lines = parts(message, 0) match {
      case Nil   => Nil
      case parts => Line(None, "") :: parts
    }
    // one gutter for the whole diagnostic, as wide as its largest line number
    val width = (formatted.excerpt.map(_.line) ++ lines.flatMap(_.at).map(_.line)).map { l => l.toString.length }.maxOption.getOrElse(0)
    val main = message.range.map(_.from.source)
    formatted.render(width) + lines.zipWithIndex.map { case (line, i) =>
      line.render(width, lines.take(i).flatMap(_.at).lastOption.map(_.source).orElse(main))
    }.mkString("")
  }

  case class Excerpt(line: Int, code: String, marker: String)

  case class Formatted(header: String, excerpt: Option[Excerpt]) {
    def render(width: Int): String = excerpt match {
      case Some(Excerpt(line, code, marker)) =>
        s"${header}\n${dim(" " * (width - line.toString.length) + line)}  ${code}\n${" " * width}  ${marker}\n"
      case None => s"${header}\n"
    }
  }

  case class Line(at: Option[Position], text: String) {
    def render(width: Int, previous: Option[kiama.util.Source]): String = at match {
      case Some(at) =>
        val location = if (previous.contains(at.source)) "" else s"   ${dim(homogenizePath(place(at)).stripSuffix(":"))}"
        s"${dim(" " * (width - at.line.toString.length) + at.line)}  ${text}${location}\n"
      case None if text.isEmpty => "\n"
      case None => s"${" " * width}  ${text}\n"
    }
  }

  // the guides of the parts a line belongs to
  private def guides(depth: Int): String = s"${dim("|")}  " * depth
  private def blank(depth: Int): Line = Line(None, List.fill(depth)(dim("|")).mkString("  "))

  private def parts(d: Diagnostic, depth: Int): List[Line] = {
    val details = d.reason.details.map { text => Line(None, guides(depth) + message(text)) }
    val rest = d.reason.alternatives.map { c => alternative(c, depth) } ++
      d.reason.causes.toList.map { e => diagnostic(e, depth) } ++
      d.subdiagnostics.map { s => subdiagnostic(s, depth) }
    details ++ spaced(rest, depth)
  }

  private def spaced(parts: List[List[Line]], depth: Int): List[Line] =
    parts.zipWithIndex.flatMap { case (lines, i) =>
      val gap = i > 0 && (lines.exists(_.at.nonEmpty) || parts(i - 1).exists(_.at.nonEmpty))
      if gap then blank(depth) :: lines else lines
    }

  private def alternative(c: Candidate, depth: Int): List[Line] = {
    val bullet = s"${guides(depth)}* "
    val declaration = for {
      name <- c.definitionTree if name.span.origin != source.Origin.Missing
      line <- excerpt(name.span.range)
    } yield c.symbol match {
      case _: symbols.BlockParam => marked(c.symbol.decl.span, bullet, s"${guides(depth)}  ", Some("is a block parameter"))
      case _ => List(Line(Some(name.span.range.from), bullet + line.code))
    }
    declaration.getOrElse(List(Line(None, bullet + message(pretty"${code(fullname(c.symbol.name))} of type ${c.tpe}")))) ++
      spaced(c.errors.toList.map { e => diagnostic(e, depth + 1) }, depth + 1)
  }

  private def diagnostic(d: Diagnostic, depth: Int): List[Line] = {
    val claim = message(d.reason.claim)
    val placed = d.primary.map(_.span).filter { span => span.origin != source.Origin.Missing }.map { span => marked(span, guides(depth), guides(depth), Some(claim)) }
    placed.filter(_.nonEmpty).getOrElse(List(Line(None, guides(depth) + claim))) ++ parts(d, depth + 1)
  }

  private def subdiagnostic(s: Subdiagnostic, depth: Int): List[Line] = s match {
    case Subdiagnostic.Note(text, at) =>
      Line(None, s"${guides(depth)}* note: ${message(text)}") :: at.toList.flatMap { span => marked(span, guides(depth + 1), guides(depth + 1), None) }
    case Subdiagnostic.Help(text) =>
      List(Line(None, s"${guides(depth)}* help: ${message(text)}"))
  }

  private def marked(span: source.Span, code: String, marker: String, label: Option[String]): List[Line] =
    excerpt(span.range).toList.flatMap { e =>
      List(Line(Some(span.range.from), code + e.code), Line(None, marker + e.marker + label.map(" " + _).getOrElse("")))
    }

  def contentWithPhase(d: Diagnostic): String = {
    val phase = d.phase.map { p => s" ${dim(s"[${p}]")}" }.getOrElse("")
    d.reason.show.map(message) match {
      case first :: rest => (title(first) + phase :: rest).mkString("\n")
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

  def excerpt(range: Range): Option[Excerpt] = {
    val from = range.from
    val to = if (range.from.line == range.to.line) range.to else from.copy(column = from.column + 1)
    from.source.optLineContents(from.line).map { src =>
      Excerpt(from.line, util.AnsiHighlight(src), (" " * (from.column - 1)) + ("^" * (to.column - from.column).max(1)))
    }
  }

  override def formatContent(err: Diagnostic): String = content(err)

  def place(p: Position): String = {
    val name = shownName(p.source)
    if (name == "") s"${p.line}:${p.column}:" else s"${name}:${p.line}:${p.column}:"
  }
}

class PlainMessaging extends ColoredMessaging {
  val escape = Style.plain

  // Don't show context in plain messaging.
  override def excerpt(range: Range): Option[Excerpt] = None
}

class AnsiColoredMessaging extends ColoredMessaging {
  val escape = Style.ansi
}

