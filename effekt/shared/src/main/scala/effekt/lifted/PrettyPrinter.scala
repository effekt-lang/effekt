package effekt
package lifted

import kiama.output.ParenPrettyPrinter

import scala.language.implicitConversions
import effekt.symbols.{ Name, Wildcard, builtins }

object PrettyPrinter extends ParenPrettyPrinter {

  import kiama.output.PrettyPrinterTypes.Document

  override val defaultIndent = 2

  def format(t: ModuleDecl): Document =
    pretty(toDoc(t), 4)

  def format(s: Stmt): String =
    pretty(toDoc(s), 60).layout

  val emptyline: Doc = line <> line

  def toDoc(m: ModuleDecl): Doc = {
    "module" <+> m.path <> emptyline <> vsep(m.imports.map { im => "import" <+> im }, line) <>
      emptyline <> toDoc(m.defs)
  }

  def toDoc(e: Extern): Doc = e match {
    case Extern.Def(id, tpe, ps, body) =>
      "extern def" <+> toDoc(id.name) <+> "=" <+> parens(hsep(ps map toDoc, comma)) <+> "=" <+> "\"" <> body <> "\""
    case Extern.Include(contents) => emptyDoc // right now, do not print includes.
  }

  def toDoc(b: Block): Doc = b match {
    case BlockVar(v) => v.name.toString
    case BlockLit(ps, body) =>
      braces { space <> parens(hsep(ps map toDoc, comma)) <+> "=>" <+> nest(line <> toDoc(body)) <> line }
    case Member(b, id) =>
      toDoc(b) <> "." <> id.name.toString
    case Unbox(e)         => parens("unbox" <+> toDoc(e))
    case New(handler)     => "new" <+> toDoc(handler)
  }

  def toDoc(p: Param): Doc = p.id.name.toString

  def toDoc(n: Name): Doc = n.toString

  def toDoc(e: Expr): Doc = e match {
    case UnitLit()               => "()"
    case StringLit(s)            => "\"" + s + "\""
    case l: Literal[t]           => l.value.toString
    case ValueVar(id)            => id.name.toString

    case PureApp(b, targs, args) => toDoc(b) <> parens(hsep(args map argToDoc, comma))

    case Select(b, field) =>
      toDoc(b) <> "." <> toDoc(field.name)

    case Closure(b) => parens("box" <+> toDoc(b))
    case Run(s, _)  => "run" <+> block(toDoc(s))
  }

  def argToDoc(e: Argument): Doc = e match {
    case e: Expr     => toDoc(e)
    case b: Block    => toDoc(b)
    case e: Evidence => toDoc(e)
  }

  def toDoc(e: Evidence): Doc = e match {
    case Evidence(Nil)  => "<>"
    case Evidence(list) => angles(hsep(list.map { ev => toDoc(ev.name) }, ","))
  }

  def toDoc(handler: Handler): Doc = {
    val handlerName = toDoc(handler.id.name)
    val clauses = handler.clauses.map {
      case (id, BlockLit(params, body)) =>
        "def" <+> toDoc(id.name) <> parens(params map toDoc) <+> "=" <+> nested(toDoc(body))
    }
    handlerName <+> block(vsep(clauses))
  }

  def toDoc(d: Decl): Doc = d match {
    case Data(did, ctors) =>
      "type" <+> toDoc(did.name) <> parens(ctors.map { id => toDoc(id.name) })

    case Record(did, fields) =>
      "record" <+> toDoc(did.name) <> parens(fields.map { f => toDoc(f.name) })

    case Interface(id, operations) =>
      "interface" <+> toDoc(id.name) <> braces(operations.map { f => toDoc(f.name) })
  }

  def toDoc(s: Stmt): Doc = s match {
    case Def(id, tpe, BlockLit(params, body), rest) =>
      "def" <+> toDoc(id.name) <> parens(params map toDoc) <+> "=" <> nested(toDoc(body)) <> emptyline <>
        toDoc(rest)

    case Def(id, tpe, b, rest) =>
      "def" <+> toDoc(id.name) <+> "=" <+> toDoc(b) <> emptyline <>
        toDoc(rest)

    case Val(Wildcard(_), tpe, binding, body) =>
      toDoc(binding) <> ";" <> line <>
        toDoc(body)

    case Val(id, tpe, binding, body) =>
      "val" <+> toDoc(id.name) <+> "=" <+> toDoc(binding) <> ";" <> line <>
        toDoc(body)

    case Let(id, tpe, binding, body) =>
      "let" <+> toDoc(id.name) <+> "=" <+> toDoc(binding) <> ";" <> line <>
        toDoc(body)

    case App(b, targs, args) =>
      toDoc(b) <> parens(hsep(args map argToDoc, comma))

    case If(cond, thn, els) =>
      "if" <+> parens(toDoc(cond)) <+> block(toDoc(thn)) <+> "else" <+> block(toDoc(els))

    case While(cond, body) =>
      "while" <+> parens(toDoc(cond)) <+> block(toDoc(body)) <+> line

    case Return(e) =>
      toDoc(e)

    case Handle(body, tpe, hs) =>
      "try" <+> toDoc(body) <+> "with" <+> hsep(hs.map(toDoc), " with")

    case State(id, init, region, body) =>
      "var" <+> toDoc(id.name) <+> "in" <+> toDoc(region.name) <+> "=" <+> toDoc(init) <+> ";" <> line <> toDoc(body)

    case Region(body) =>
      "region" <+> toDoc(body)

    case Match(sc, clauses, default) =>
      val cs = braces(nest(line <> vsep(clauses map { case (p, b) => "case" <+> toDoc(p.name) <+> toDoc(b) })) <> line)
      val d = default.map { body => space <> "else" <+> braces(nest(line <> toDoc(body))) }.getOrElse { emptyDoc }
      toDoc(sc) <+> "match" <+> cs <> d

    case Hole =>
      "<>"
  }

  def nested(content: Doc): Doc = group(nest(line <> content))

  def parens(docs: List[Doc]): Doc = parens(hsep(docs, comma))

  def braces(docs: List[Doc]): Doc = braces(hsep(docs, semi))

  def brackets(docs: List[Doc]): Doc = brackets(hsep(docs, comma))

  def block(content: Doc): Doc = braces(nest(line <> content) <> line)

  def block(docs: List[Doc]): Doc = block(vsep(docs, line))

}
