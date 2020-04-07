package effekt
package core

import org.bitbucket.inkytonik.kiama.output.ParenPrettyPrinter

import scala.language.implicitConversions
import effekt.symbols.{ builtins, moduleName, moduleFile, Name }

class PrettyPrinter extends ParenPrettyPrinter {

  import org.bitbucket.inkytonik.kiama.output.PrettyPrinterTypes.Document

  def format(t: ModuleDecl): Document =
    pretty(toDoc(t), 4)

  val emptyline: Doc = line <> line

  def toDoc(m: ModuleDecl): Doc = {
    "module" <+> m.path <> emptyline <> vsep(m.imports.map { im => "import" <+> im }, line) <>
      emptyline <> toDocStmt(m.defs)
  }

  def toDoc(b: Block): Doc = b match {
    case BlockVar(v) => v.name.toString
    case BlockDef(ps, body) =>
      parens(hsep(ps map toDoc, comma)) <+> "=>" <+> braces(nest(line <> toDoc(body)) <> line)
    case Lift(b)          => parens(toDoc(b))
    case Extern(ps, body) => parens(hsep(ps map toDoc, comma)) <+> "=>" <+> braces(nest(line <> body) <> line)
  }

  def toDoc(p: Param): Doc = p.id.name.toString

  def toDoc(n: Name): Doc = n.toString

  def toDoc(e: Expr): Doc = e match {
    case UnitLit()     => "()"
    case StringLit(s)  => "\"" + s + "\""
    case l: Literal[t] => l.value.toString
    case ValueVar(id)  => id.name.toString

    case Deref(id)     => "!" <> toDoc(id.name)
    case Assign(id, e) => toDoc(id.name) <+> ":=" <+> toDoc(e)

    case PureApp(b, args) => toDoc(b) <> parens(hsep(args map {
      case e: Expr  => toDoc(e)
      case b: Block => toDoc(b)
    }, comma))
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
    case Val(Wildcard(_), binding, body) =>
      toDoc(binding) <> ";" <> line <> toDoc(body)
    case Val(id, binding, body) =>
      "val" <+> toDoc(id.name) <+> "=" <+> toDoc(binding) <> ";" <> line <> toDoc(body)
    case Var(id, binding, body) =>
      "var" <+> toDoc(id.name) <+> "=" <+> toDoc(binding) <> ";" <> line <> toDoc(body)
    case App(b, args) =>
      toDoc(b) <> parens(hsep(args map argToDoc, comma))
    case If(cond, thn, els) =>
      "if" <+> parens(toDoc(cond)) <+> toDocExpr(thn) <+> "else" <+> toDocExpr(els)
    case While(cond, body) =>
      "while" <+> parens(toDoc(cond)) <+> braces(nest(line <> toDoc(body)) <+> line)
    case Ret(e) =>
      toDoc(e)
    // don't print exports for now
    case Exports(path, exports) =>
      emptyDoc
    case Handle(body, handler) =>
      val cs = vsep(handler.clauses map { case (n, b) => "case" <+> toDoc(n.name) <> toDoc(b) })
      "handle" <+> braces(nest(line <+> toDoc(body)) <> line) <+> "with" <+> braces(nest(line <+> cs) <> line)
    case Match(sc, clauses) =>
      val cs = braces(nest(line <> vsep(clauses map { case (id, b) => "case" <+> toDoc(id.name) <> toDoc(b) })) <> line)
      toDoc(sc) <+> "match" <+> cs
    case other =>
      sys error s"Cannot pretty print $other in expression position"
  }

  def toDocStmt(s: Stmt): Doc = s match {
    case Def(id, Extern(ps, body), rest) =>
      "extern def" <+> toDoc(id.name) <+> "=" <+> parens(hsep(ps map toDoc, comma)) <+> "=>" <+>
        braces(nest(body) <> line) <> emptyline <> toDocStmt(rest)

    case Def(id, b, rest) =>
      "def" <+> toDoc(id.name) <+> "=" <+> toDoc(b) <> emptyline <> toDocStmt(rest)

    case Data(did, ctors, rest) =>
      val cs = ctors.map { id => toDoc(id.name) }
      "type" <+> toDoc(did.name) <> parens(hsep(cs, ",")) <> emptyline <> toDocStmt(rest)

    // for now, don't print includes
    case Include(contents, rest) =>
      toDocStmt(rest)

    case other => toDocExpr(other)
  }

  def requiresBlock(s: Stmt): Boolean = s match {
    case Data(did, ctors, rest) => true
    case Def(id, d, rest) => true
    case _ => false
  }

}
