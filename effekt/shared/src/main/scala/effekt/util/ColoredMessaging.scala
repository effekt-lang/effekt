package effekt
package util

import effekt.util.messages.*
import kiama.util.{ Position, Source }

trait ColoredMessaging extends EffektMessaging with Style {
  import Drawing.*

  override def formatMessage(message: Diagnostic): String = render(Drawing(message))

  def showsCode: Boolean = true

  // Where it is drawn
  // -----------------

  // precedes a line's content
  enum Lead {
    case Bullet(kind: ItemKind)
    case Hang
  }

  enum Line {
    case Code(at: Position, depth: Int, lead: Option[Lead], segments: List[Segment])
    case Text(depth: Int, lead: Option[Lead], segments: List[Segment])
    case Blank(depth: Int)
  }

  case class Segment(text: String, role: Option[Role])

  // the lines drawn so far + where the next one goes
  private final class Lines(val gutter: Int) {
    private[ColoredMessaging] val drawn = scala.collection.mutable.ListBuffer.empty[Line]
    private[ColoredMessaging] var depth = 0
    private[ColoredMessaging] var lead: Option[Lead] = None
  }
  private object Lines {
    def apply(gutter: Int)(block: Lines ?=> Unit): List[Line] = {
      val L = new Lines(gutter)
      block(using L)
      L.drawn.toList
    }
  }

  private def lead(l: Lead): String = l match {
    case Lead.Bullet(kind) => "* " + kind.word
    case Lead.Hang         => "  "
  }

  private def emit(line: Option[Lead] => Line)(using L: Lines): Unit = {
    L.drawn += line(L.lead)
    L.lead = L.lead.map { _ => Lead.Hang }
  }

  private def text(segments: List[Segment])(using L: Lines): Unit =
    emit { lead => Line.Text(L.depth, lead, segments) }
  private def code(at: Position, segments: List[Segment])(using L: Lines): Unit =
    emit { lead => Line.Code(at, L.depth, lead, segments) }
  private def blank()(using L: Lines): Unit = L.drawn += Line.Blank(L.depth)

  private def deeper(block: Lines ?=> Unit)(using L: Lines): Unit = {
    val (depth, lead) = (L.depth, L.lead)
    L.depth = depth + 1; L.lead = None
    block
    L.depth = depth; L.lead = lead
  }

  // its bullet on its first line
  private def item(kind: ItemKind)(block: Lines ?=> Unit)(using L: Lines): Unit = {
    L.lead = Some(Lead.Bullet(kind))
    block
    L.lead = None
  }

  // a blank line between a node's place and its children, if it has any
  private def children(block: Lines ?=> Unit)(using L: Lines): Unit = {
    val (before, depth) = (L.drawn.size, L.depth)
    block
    if (L.drawn.size > before) L.drawn.insert(before, Line.Blank(depth))
  }

  private def plain(s: String): List[Segment] = if (s.isEmpty) Nil else List(Segment(s, None))
  private def as(role: Role, s: String): List[Segment] = if (s.isEmpty) Nil else List(Segment(s, Some(role)))

  private def segments(m: StructuredMessage): List[Segment] = m.pieces.flatMap {
    case Piece.Code(s) => as(Role.Code, s)
    case Piece.Text(s) => plain(s)
  }

  private def highlighted(code: String): List[Segment] = Highlight.tokens(code).flatMap {
    case (s, Some(highlight)) => as(Role.Token(highlight), s)
    case (s, None)            => plain(s)
  }

  // the places whose code this output shows
  private def shown(excerpts: List[Excerpt]): List[Excerpt] = if (showsCode) excerpts else Nil

  private def excerpts(body: Body): List[Excerpt] = body.blocks.flatMap(excerpts)

  private def excerpts(block: Block): List[Excerpt] = block match {
    case Block.Item(_, _, excerpts, body) => shown(excerpts) ++ this.excerpts(body)
    case Block.Nested(_, excerpts, body)  => shown(excerpts) ++ this.excerpts(body)
  }

  // as wide as the largest line number
  private def gutter(d: Drawing): Int =
    (shown(d.excerpts) ++ excerpts(d.body)).map { e => e.at.line.toString.length }.maxOption.getOrElse(0)

  def layout(d: Drawing): List[Line] = Lines(gutter(d)) {
    shown(d.excerpts).foreach { e => layout(e, None) }
    children { layout(d.body) }
  }

  // a blank line between consecutive items where either spans several lines
  private def layout(body: Body)(using Lines): Unit = {
    body.introduction.foreach { m => text(segments(m)) }
    separated(body.blocks) { (previous, block) => if (spans(previous) || spans(block)) blank() } { block =>
      layout(block)
    }
  }

  private def spans(block: Block): Boolean = excerpts(block).nonEmpty

  private def layout(block: Block)(using Lines): Unit = block match {
    case Block.Item(kind, m, excerpts, body) => shown(excerpts) match {
      case declaration :: rest if kind == ItemKind.Candidate =>
        item(kind) { layout(declaration, None) }
        deeper { rest.foreach { e => layout(e, None) }; layout(body) }
      case places =>
        item(kind) { text(segments(m)) }
        deeper { places.foreach { e => layout(e, None) }; layout(body) }
    }

    // a nested claim hangs from the mark at its first place
    case Block.Nested(claim, excerpts, body) =>
      shown(excerpts) match {
        case first :: rest => layout(first, Some(claim)); rest.foreach { e => layout(e, None) }
        case Nil           => text(segments(claim))
      }
      children { deeper { layout(body) } }
  }

  private def layout(e: Excerpt, claim: Option[StructuredMessage])(using Lines): Unit =
    e.at.source.optLineContents(e.at.line).foreach { line =>
      code(e.at, highlighted(line))
      e.labels.foreach { l => marker(l.span, claim) }
    }

  private def marker(span: source.Span, label: Option[StructuredMessage])(using Lines): Unit = {
    val from = span.range.from
    val to = if (from.line == span.range.to.line) span.range.to else from.copy(column = from.column + 1)
    val carets = plain(" " * (from.column - 1) + "^" * (to.column - from.column).max(1))
    text(carets ++ label.toList.flatMap { m => plain(" ") ++ segments(m) })
  }

  // What it looks like
  // ------------------

  private def show(segments: List[Segment]): String =
    segments.map { case Segment(s, role) => role.map { r => styled(r, s) }.getOrElse(s) }.mkString

  def render(d: Drawing): String = {
    val gutter = this.gutter(d)
    def dim(s: String) = styled(Role.Dim, s)
    def guides(depth: Int) = s"${dim("|")}  " * depth
    def leading(l: Option[Lead]) = l.map(lead).getOrElse("")

    def rendered(lines: List[Line], previous: Option[Source]): List[String] = lines match {
      case Nil => Nil
      case Line.Code(at, depth, l, segments) :: rest =>
        val location = if (previous.contains(at.source)) "" else s"   ${dim(homogenizePath(place(at)).stripSuffix(":"))}"
        val number = dim(" " * (gutter - at.line.toString.length) + at.line)
        s"${number}  ${guides(depth)}${leading(l)}${show(segments)}${location}" :: rendered(rest, Some(at.source))
      case Line.Text(depth, l, segments) :: rest =>
        s"${" " * gutter}  ${guides(depth)}${leading(l)}${show(segments)}" :: rendered(rest, previous)
      case Line.Blank(0) :: rest =>
        "" :: rendered(rest, previous)
      case Line.Blank(depth) :: rest =>
        s"${" " * gutter}  ${List.fill(depth)(dim("|")).mkString("  ")}" :: rendered(rest, previous)
    }
    show(heading(d.header)) + "\n" + rendered(layout(d), d.header.at.map(_.source)).map(_ + "\n").mkString
  }

  private def heading(h: Header): List[Segment] = {
    val severity = plain("[") ++ as(Role.Severity(h.severity), severityWord(h.severity)) ++ plain("] ")
    val place = h.at.toList.flatMap { at => plain(homogenizePath(this.place(at)) + " ") }
    val phase = h.phase.toList.flatMap { p => plain(" ") ++ as(Role.Dim, s"[${p}]") }
    val text = h.text match {
      case first :: rest =>
        val title = segments(first).map { s => Segment(s.text, s.role.orElse(Some(Role.Title))) }
        title ++ phase ++ rest.flatMap { m => plain("\n") ++ segments(m) }
      case Nil => phase
    }
    severity ++ place ++ text
  }

  private def severityWord(severity: Severity): String = severity match {
    case Severity.Error   => "error"
    case Severity.Warning => "warning"
    case Severity.Info    => "info"
    case Severity.Hint    => "hint"
  }

  def content(d: Diagnostic): String = d.reason.show.map(message).mkString("\n")

  def contentWithSubdiagnostics(d: Diagnostic): String = contentWithSubdiagnostics(Drawing(d))

  // the body one level deeper, so that the header stands apart
  def contentWithSubdiagnostics(drawing: Drawing): String = {
    val lines = scala.collection.mutable.ListBuffer.empty[String]
    def body(b: Drawing.Body, depth: Int): Unit = {
      b.introduction.foreach { m => lines += "  " * depth + message(m) }
      b.blocks.foreach {
        case Drawing.Block.Item(kind, m, _, items) =>
          lines += "  " * depth + "* " + kind.word + message(m)
          body(items, depth + 1)
        case Drawing.Block.Nested(claim, _, nested) =>
          lines += "  " * depth + message(claim)
          body(nested, depth + 1)
      }
    }
    drawing.header.text.foreach { m => lines += message(m) }
    body(drawing.body, 1)
    lines.mkString("\n")
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
  override def showsCode = false
}

class AnsiColoredMessaging extends ColoredMessaging {
  val escape = Style.ansi
}
