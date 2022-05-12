package effekt
package core

import kiama.output.ParenPrettyPrinter

import scala.language.implicitConversions
import effekt.symbols.{ builtins, Name, Wildcard }

class PrettyPrinter extends ParenPrettyPrinter {

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
    case Unbox(e)         => toDoc(e)
  }

  def toDoc(p: Param): Doc = p.id.name.toString

  def toDoc(n: Name): Doc = n.toString

  def toDoc(e: Expr): Doc = e match {
    case UnitLit()     => "()"
    case StringLit(s)  => "\"" + s + "\""
    case l: Literal[t] => l.value.toString
    case ValueVar(id)  => id.name.toString

    case PureApp(b, targs, args) => toDoc(b) <> parens(hsep(args map {
      case e: Expr  => toDoc(e)
      case b: Block => toDoc(b)
    }, comma))

    case Select(b, field) =>
      toDoc(b) <> "." <> toDoc(field.name)

    case Box(b) => toDoc(b)
    case Run(s) => "run" <+> braces(toDoc(s)) <+> "}"
  }

  def argToDoc(e: Argument): Doc = e match {
    case e: Expr  => toDoc(e)
    case b: Block => toDoc(b)
  }

  def toDoc(s: Stmt): Doc =
    if (requiresBlock(s))
      braces(nest(line <> toDocStmt(s)) <> line)
    else
      toDocExpr(s)

  // pretty print the statement in a javascript expression context
  // not all statement types can be printed in this context!
  def toDocExpr(s: Stmt): Doc = s match {
    case Val(Wildcard(_), tpe, binding, body) =>
      toDoc(binding) <> ";" <> line <> toDoc(body)
    case Val(id, tpe, binding, body) =>
      "val" <+> toDoc(id.name) <+> ":" <+> tpe.toString <+> "=" <+> toDoc(binding) <> ";" <> line <> toDoc(body)
    case App(b, targs, args) =>
      toDoc(b) <> parens(hsep(args map argToDoc, comma))
    case If(cond, thn, els) =>
      "if" <+> parens(toDoc(cond)) <+> toDocExpr(thn) <+> "else" <+> toDocExpr(els)
    case While(cond, body) =>
      "while" <+> parens(toDoc(cond)) <+> braces(nest(line <> toDoc(body)) <+> line)
    case Ret(e) =>
      toDoc(e)
    case Handle(body, hs) =>
      // TODO pretty print correctly
      val handlers = hs map { handler =>
        braces(nest(line <> vsep(handler.clauses.map { case (id, b) => toDoc(id.name) <> ":" <+> toDoc(b) }, comma)) <> line)
      }
      val cs = parens("[" <> hsep(handlers, comma) <> "]")
      "handle" <+> braces(nest(line <> toDoc(body)) <> line) <+> "with" <+> cs
    case State(id, tpe, get, put, init, body) =>
      "state" <+> parens(toDoc(init)) <+> braces(nest(line <> toDoc(body)) <> line)

    case Match(sc, clauses) =>
      val cs = braces(nest(line <> vsep(clauses map { case (p, b) => "case" <+> toDoc(p) <+> "=>" <+> toDoc(b) })) <> line)
      toDoc(sc) <+> "match" <+> cs
    case Hole =>
      "<>"
    case other =>
      sys error s"Cannot pretty print $other in expression position"
  }

  def toDoc(p: Pattern): Doc = p match {
    case IgnorePattern()          => "_"
    case LiteralPattern(l)        => toDoc(l)
    case AnyPattern()             => "*"
    case TagPattern(id, patterns) => toDoc(id.name) <> parens(hsep(patterns map toDoc, comma))
  }

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

    // for now, don't print includes
    case Include(contents, rest) =>
      toDocStmt(rest)

    case other => toDocExpr(other)
  }

  def requiresBlock(s: Stmt): Boolean = s match {
    case Data(did, ctors, rest) => true
    case Def(id, tpe, d, rest) => true
    case _ => false
  }

}
