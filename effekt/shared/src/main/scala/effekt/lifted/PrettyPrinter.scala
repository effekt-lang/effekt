package effekt
package lifted

import kiama.output.ParenPrettyPrinter

import scala.language.implicitConversions
import effekt.symbols.{ Name, TypePrinter, Wildcard, builtins }

object PrettyPrinter extends ParenPrettyPrinter {

  import kiama.output.PrettyPrinterTypes.Document

  def format(t: ModuleDecl): Document =
    pretty(toDoc(t), 4)

  val emptyline: Doc = line <> line

  def toDoc(m: ModuleDecl): Doc = {
    "module" <+> m.path <> emptyline <> vsep(m.imports.map { im => "import" <+> im }, line) <>
      emptyline <> toDocStmt(m.defs)
  }

  def toDoc(b: Block): Doc = b match {
    case BlockVar(v) => v.name.toString
    case BlockLit(ps, body) =>
      parens(hsep(ps map toDoc, comma)) <+> "=>" <+> braces(nest(line <> toDoc(body)) <> line)
    case Member(b, id) =>
      toDoc(b) <> "." <> id.name.toString
    case Extern(ps, body) => parens(hsep(ps map toDoc, comma)) <+> "=>" <+> braces(nest(line <> body) <> line)
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
    case Run(s)     => "run" <+> braces(toDoc(s))
  }

  def argToDoc(e: Argument): Doc = e match {
    case e: Expr     => toDoc(e)
    case b: Block    => toDoc(b)
    case e: Evidence => toDoc(e)
  }

  def toDoc(e: Evidence): Doc = e match {
    case Evidence(Nil)  => "Here"
    case Evidence(list) => hsep(list.map { ev => toDoc(ev.name) }, "+")
  }

  def toDoc(s: Stmt): Doc =
    if (requiresBlock(s))
      braces(nest(line <> toDocStmt(s)) <> line)
    else
      toDocStmt(s)

  def toDoc(p: Pattern): Doc = p match {
    case IgnorePattern()          => "_"
    case LiteralPattern(l)        => toDoc(l)
    case AnyPattern()             => "*"
    case TagPattern(id, patterns) => toDoc(id.name) <> parens(hsep(patterns map toDoc, comma))
  }

  def toDoc(handler: Handler): Doc = braces(nest(line <> vsep(handler.clauses.map { case (id, b) => toDoc(id.name) <> ":" <+> toDoc(b) }, comma)) <> line)

  def toDocStmt(s: Stmt): Doc = s match {
    case Def(id, tpe, Extern(ps, body), rest) =>
      "extern def" <+> toDoc(id.name) <+> "=" <+> parens(hsep(ps map toDoc, comma)) <+> "=>" <+>
        braces(nest(body) <> line) <> emptyline <> toDocStmt(rest)

    case Def(id, tpe, b, rest) =>
      "def" <+> toDoc(id.name) <+> "=" <+> toDoc(b) <> emptyline <> toDocStmt(rest)

    case Data(did, ctors, rest) =>
      val cs = ctors.map { id => toDoc(id.name) }
      "type" <+> toDoc(did.name) <> parens(hsep(cs, ",")) <> emptyline <> toDocStmt(rest)

    case Record(did, fields, rest) =>
      val fs = fields.map { f => toDoc(f.name) }
      "record" <+> toDoc(did.name) <> parens(hsep(fs, ",")) <> emptyline <> toDocStmt(rest)

    case Val(Wildcard(_), tpe, binding, body) =>
      toDoc(binding) <> ";" <> line <> toDoc(body)

    case Val(id, tpe, binding, body) =>
      "val" <+> toDoc(id.name) <+> ":" <+> TypePrinter.show(tpe) <+> "=" <+> toDoc(binding) <> ";" <> line <> toDoc(body)

    case Let(id, tpe, binding, body) =>
      "let" <+> toDoc(id.name) <+> ":" <+> TypePrinter.show(tpe) <+> "=" <+> toDoc(binding) <> ";" <> line <> toDoc(body)

    case App(b, targs, args) =>
      toDoc(b) <> parens(hsep(args map argToDoc, comma))

    case If(cond, thn, els) =>
      "if" <+> parens(toDoc(cond)) <+> toDocStmt(thn) <+> "else" <+> toDocStmt(els)

    case While(cond, body) =>
      "while" <+> parens(toDoc(cond)) <+> braces(nest(line <> toDoc(body)) <+> line)

    case Return(e) =>
      "return" <+> toDoc(e)

    case Handle(body, hs) =>
      // TODO pretty print correctly
      val handlers = hs.map(toDoc)
      val cs = parens("[" <> hsep(handlers, comma) <> "]")
      "handle" <+> braces(nest(line <> toDoc(body)) <> line) <+> "with" <+> cs

    case State(id, init, region, body) =>
      "var" <+> toDoc(id.name) <+> "in" <+> toDoc(region.name) <+> "=" <+> toDoc(init) <+> ";" <> line <> toDoc(body)

    case Region(body) =>
      "region" <+> toDoc(body)

    case Match(sc, clauses) =>
      val cs = braces(nest(line <> vsep(clauses map { case (p, b) => "case" <+> toDoc(p) <+> "=>" <+> toDoc(b) })) <> line)
      toDoc(sc) <+> "match" <+> cs

    case Hole =>
      "<>"

    // for now, don't print includes
    case Include(contents, rest) =>
      toDocStmt(rest)
  }

  def requiresBlock(s: Stmt): Boolean = s match {
    case Data(did, ctors, rest) => true
    case Def(id, tpe, d, rest) => true
    case _ => false
  }

}
