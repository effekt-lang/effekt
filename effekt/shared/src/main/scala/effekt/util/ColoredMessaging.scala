package effekt
package util

import effekt.util.messages.*
import kiama.util.{ Position, Source }

case class Drawing(header: Drawing.Header, excerpts: List[Drawing.Excerpt], body: Drawing.Body)

object Drawing {
  case class Header(severity: Severity, at: Option[Position], text: List[StructuredMessage], phase: Option[String])

  case class Body(introduction: List[StructuredMessage], blocks: List[Block]) {
    def isEmpty: Boolean = introduction.isEmpty && blocks.isEmpty
  }
  object Body { val empty = Body(Nil, Nil) }

  enum Block {
    case Item(kind: ItemKind, text: Option[StructuredMessage], excerpts: List[Excerpt], body: Body)
    case Nested(claim: StructuredMessage, excerpts: List[Excerpt], body: Body)
  }

  case class Excerpt(at: Position, labels: List[Label])

  enum ItemKind { case Candidate, Note, Help }
}

trait ColoredMessaging extends EffektMessaging with Style {
  import Drawing.*

  override def formatMessage(message: Diagnostic): String = render(draw(message))

  // What is drawn
  // -------------

  def draw(d: Diagnostic): Drawing =
    Drawing(Header(d.severity, d.range.map(_.from), d.reason.show, d.phase), places(d), body(d))

  private def places(d: Diagnostic): List[Excerpt] =
    d.primary.toList.flatMap { label => excerpt(label.span) }

  private def body(d: Diagnostic): Body = {
    val blocks = d.reason.alternatives.map(block) ++ d.reason.causes.toList.map(block) ++ d.subdiagnostics.map(block)
    Body(d.reason.details, blocks)
  }

  private def block(c: Candidate): Block = {
    val declaration = c.definitionTree.toList.flatMap { name => excerpt(name.span) }
    val name = Piece.Code(fullname(c.symbol.name))
    val text = if declaration.isEmpty then Some(pretty"${name} of type ${c.tpe}") else None
    Block.Item(ItemKind.Candidate, text, declaration, Body(Nil, c.errors.toList.map(block)))
  }

  private def block(d: Diagnostic): Block =
    Block.Nested(d.reason.claim, places(d), body(d))

  private def block(s: Subdiagnostic): Block = s match {
    case Subdiagnostic.Note(text, at) => Block.Item(ItemKind.Note, Some(text), at.toList.flatMap(excerpt), Body.empty)
    case Subdiagnostic.Help(text)     => Block.Item(ItemKind.Help, Some(text), Nil, Body.empty)
  }

  // the code at a span, marked
  def excerpt(span: source.Span): Option[Excerpt] = {
    val at = span.range.from
    if span.origin == source.Origin.Missing then None
    else at.source.optLineContents(at.line).map { _ => Excerpt(at, List(Label(span))) }
  }

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
    case Lead.Bullet(ItemKind.Candidate) => "* "
    case Lead.Bullet(ItemKind.Note)      => "* note: "
    case Lead.Bullet(ItemKind.Help)      => "* help: "
    case Lead.Hang                       => "  "
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

  private def segments(m: StructuredMessage): List[Segment] = pieces(m).flatMap {
    case Piece.Code(s) => as(Role.Code, s)
    case Piece.Text(s) => plain(s)
  }

  private def highlighted(code: String): List[Segment] = AnsiHighlight.tokens(code).flatMap {
    case (s, Some(highlight)) => as(Role.Token(highlight), s)
    case (s, None)            => plain(s)
  }

  private def excerpts(body: Body): List[Excerpt] = body.blocks.flatMap(excerpts)

  private def excerpts(block: Block): List[Excerpt] = block match {
    case Block.Item(_, _, excerpts, body) => excerpts ++ this.excerpts(body)
    case Block.Nested(_, excerpts, body)  => excerpts ++ this.excerpts(body)
  }

  // as wide as the largest line number
  private def gutter(d: Drawing): Int =
    (d.excerpts ++ excerpts(d.body)).map { e => e.at.line.toString.length }.maxOption.getOrElse(0)

  def layout(d: Drawing): List[Line] = Lines(gutter(d)) {
    d.excerpts.foreach { e => layout(e, None) }
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
    // an item's text with its places one level deeper
    case Block.Item(kind, Some(m), excerpts, body) =>
      item(kind) { text(segments(m)) }
      deeper { excerpts.foreach { e => layout(e, None) }; layout(body) }

    case Block.Item(kind, None, excerpts, body) =>
      item(kind) { excerpts.foreach { e => layout(e, None) } }
      deeper { layout(body) }

    // a nested claim hangs from the mark at its first place
    case Block.Nested(claim, excerpts, body) =>
      excerpts match {
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

  def lspSource(d: Diagnostic): String = d.phase match {
    case Some(phase) => s"effekt [${phase}]"
    case None => "effekt"
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
  override def excerpt(span: source.Span): Option[Drawing.Excerpt] = None
}

class AnsiColoredMessaging extends ColoredMessaging {
  val escape = Style.ansi
}
