package effekt
package util

import effekt.util.messages.*
import kiama.util.Position

case class Drawing(header: Drawing.Header, excerpts: List[Drawing.Excerpt], body: Drawing.Body)

object Drawing {
  case class Header(severity: Severity, at: Option[Position], text: List[StructuredMessage], phase: Option[String])

  case class Body(introduction: List[StructuredMessage], blocks: List[Block])
  object Body { val empty = Body(Nil, Nil) }

  enum Block {
    case Item(kind: ItemKind, text: StructuredMessage, excerpts: List[Excerpt], body: Body)
    case Nested(claim: StructuredMessage, excerpts: List[Excerpt], body: Body)
  }

  case class Excerpt(at: Position, labels: List[Label])

  enum ItemKind {
    case Candidate, Note, Help

    def word: String = this match {
      case Candidate => ""
      case Note      => "note: "
      case Help      => "help: "
    }
  }

  def apply(d: Diagnostic): Drawing = Drawing(header(d), places(d), body(d))

  def header(d: Diagnostic): Header = Header(d.severity, d.range.map(_.from), d.reason.show, d.phase)

  private def places(d: Diagnostic): List[Excerpt] =
    d.primary.toList.flatMap { label => excerpt(label.span) }

  private def body(d: Diagnostic): Body = {
    val blocks = d.reason.alternatives.map(block) ++ d.reason.causes.toList.map(block) ++ d.subdiagnostics.map(block)
    Body(d.reason.details, blocks)
  }

  private def block(c: Candidate): Block = {
    val declaration = c.definitionTree.toList.flatMap { name => excerpt(name.span) }
    Block.Item(ItemKind.Candidate, c.show, declaration, Body(Nil, c.errors.toList.map(block)))
  }

  private def block(d: Diagnostic): Block =
    Block.Nested(d.reason.claim, places(d), body(d))

  private def block(s: Subdiagnostic): Block = s match {
    case Subdiagnostic.Note(text, at) => Block.Item(ItemKind.Note, text, at.toList.flatMap(excerpt), Body.empty)
    case Subdiagnostic.Help(text)     => Block.Item(ItemKind.Help, text, Nil, Body.empty)
  }

  // the code at a span with markings
  private def excerpt(span: source.Span): Option[Excerpt] = {
    val at = span.range.from
    if span.isMissing then None
    else at.source.optLineContents(at.line).map { _ => Excerpt(at, List(Label(span))) }
  }
}

// the body one level deeper, so that the header stands apart
def contentWithSubdiagnostics(drawing: Drawing, style: Style): String = {
  val lines = scala.collection.mutable.ListBuffer.empty[String]
  def body(b: Drawing.Body, depth: Int): Unit = {
    b.introduction.foreach { m => lines += "  " * depth + style.message(m) }
    b.blocks.foreach {
      case Drawing.Block.Item(kind, m, _, items) =>
        lines += "  " * depth + "* " + kind.word + style.message(m)
        body(items, depth + 1)
      case Drawing.Block.Nested(claim, _, nested) =>
        lines += "  " * depth + style.message(claim)
        body(nested, depth + 1)
    }
  }
  drawing.header.text.foreach { m => lines += style.message(m) }
  body(drawing.body, 1)
  lines.mkString("\n")
}
