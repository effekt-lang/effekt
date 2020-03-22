package effekt
package core

import org.bitbucket.inkytonik.kiama.output.ParenPrettyPrinter

import scala.language.implicitConversions
import effekt.symbols.{ builtins, moduleName, moduleFile, Name }

class JavaScript extends ParenPrettyPrinter {

  import org.bitbucket.inkytonik.kiama.output.PrettyPrinterTypes.Document

  def format(t : ModuleDecl): Document =
        pretty(toDoc(t), 4)

  val prelude = "if (typeof define !== 'function') { var define = require('amdefine')(module) }"

  val emptyline: Doc = line <> line

  def toDoc(m: ModuleDecl): Doc = {
    val deps = m.imports
    val imports = brackets(hsep(deps.map { i => "'./" + moduleFile(i) + "'" }, comma))
    val moduleDecl = "var" <+> moduleName(m.path) <+> "=" <+> "{};"
    prelude <> line <> "define" <> parens(imports <> comma <+> "function" <> parens(hsep(deps.map { d => moduleName(d) }, comma)) <+>
      braces(nest(line <> moduleDecl <> emptyline <> toDocStmt(m.defs)) <> line))
  }

  def toDoc(b: Block): Doc = b match {
    case BlockVar(v) => v.name.qualified
    case BlockDef(ps, body) =>
      parens(hsep(ps map toDoc, comma)) <+> "=>" <+> toDoc(body)
    case Lift(b) => "$effekt.lift" <> parens(toDoc(b))
    case Extern(ps, body) =>
      parens(hsep(ps map toDoc, comma)) <+> "=>" <+> body
  }

  def toDoc(p: Param): Doc = p.id.name.toString

  def toDoc(n: Name): Doc = n.toString

  def toDoc(e: Expr): Doc = e match {
    case UnitLit() => "null"
    case StringLit(s) => "\"" + s + "\""
    case l: Literal[t] => l.value.toString
    case ValueVar(id) => id.name.qualified

    case Deref(id) => toDoc(id.name) <> ".value()"
    case Assign(id, e) => toDoc(id.name) <> ".value" <> parens(toDoc(e))

    case PureApp(BlockVar(builtins.infixAdd), List(left: Expr, right: Expr)) =>
      parens(toDoc(left)) <+> "+" <+> parens(toDoc(right))

    case PureApp(BlockVar(builtins.infixMul), List(left: Expr, right: Expr)) =>
      parens(toDoc(left)) <+> "*" <+> parens(toDoc(right))

    case PureApp(BlockVar(builtins.infixSub), List(left: Expr, right: Expr)) =>
      parens(toDoc(left)) <+> "-" <+> parens(toDoc(right))

    case PureApp(BlockVar(builtins.infixDiv), List(left: Expr, right: Expr)) =>
      parens(toDoc(left)) <+> "-" <+> parens(toDoc(right))

    case PureApp(BlockVar(builtins.infixLte), List(left: Expr, right: Expr)) =>
      parens(toDoc(left)) <+> "<=" <+> parens(toDoc(right))

    case PureApp(BlockVar(builtins.infixLt), List(left: Expr, right: Expr)) =>
      parens(toDoc(left)) <+> "<" <+> parens(toDoc(right))

    case PureApp(BlockVar(builtins.infixGte), List(left: Expr, right: Expr)) =>
      parens(toDoc(left)) <+> ">=" <+> parens(toDoc(right))

    case PureApp(BlockVar(builtins.infixGt), List(left: Expr, right: Expr)) =>
      parens(toDoc(left)) <+> ">" <+> parens(toDoc(right))

    case PureApp(BlockVar(builtins.infixOr), List(left: Expr, right: Expr)) =>
      parens(toDoc(left)) <+> "||" <+> parens(toDoc(right))

    case PureApp(BlockVar(builtins.infixAnd), List(left: Expr, right: Expr)) =>
      parens(toDoc(left)) <+> "&&" <+> parens(toDoc(right))

    case PureApp(BlockVar(builtins.not), List(expr: Expr)) =>
      "!" <+> parens(toDoc(expr))

    case PureApp(b, args) => toDoc(b) <> parens(hsep(args map {
      case e: Expr => toDoc(e)
      case b: Block => toDoc(b)
    }, comma))
  }

  def argToDoc(e: Argument): Doc = e match {
    case e: Expr => toDoc(e)
    case b: Block => toDoc(b)
  }

  def toDoc(s: Stmt): Doc =
    if (requiresBlock(s))
      braces(nest(line <> toDocStmt(s)) <> line)
    else
      toDocExpr(s)

  def toDocDelayed(s: Stmt): Doc =
    if (requiresBlock(s))
      "$effekt.delayed" <> parens("() => " <+> braces(nest(line <> toDocStmt(s)) <> line))
    else
      toDocExpr(s)

  // pretty print the statement in a javascript expression context
  // not all statement types can be printed in this context!
  def toDocExpr(s: Stmt): Doc = s match {
    case Val(Wildcard, binding, body) =>
      toDocDelayed(binding) <> ".then" <> parens("()" <+> "=>" <+> nest(line <> toDoc(body)))
    case Val(id, binding, body) =>
      toDocDelayed(binding) <> ".then" <> parens(toDoc(id.name) <+> "=>" <+> nest(line <> toDoc(body)))
    case Var(id, binding, body) =>
      toDocDelayed(binding) <> ".state" <> parens(toDoc(id.name) <+> "=>" <+> nest(line <> toDoc(body)))
    case App(b, args) =>
      toDoc(b) <> parens(hsep(args map argToDoc, comma))
    case If(cond, thn, els) =>
      parens(toDoc(cond)) <+> "?" <+> toDocDelayed(thn) <+> ":" <+> toDocDelayed(els)
    case While(cond, body) =>
      "$effekt._while" <> parens(
        "() =>" <+> toDoc(cond) <> comma <+>
        "() =>" <+> toDoc(body))
    case Ret(e) =>
      "$effekt.pure" <> parens(toDoc(e))
    case Exports(path, exports) =>
      "Object.assign" <> parens(moduleName(path) <> comma <+> braces(nest(line <> vsep(exports.map { e =>
        toDoc(e.name) <> ":" <+> toDoc(e.name)
      }, comma)) <> line))
    case Handle(body, clauses) =>
      val cs = parens("[" <> nest(line <> vsep(clauses map { case (_, b) => toDoc(b) }, comma)) <> "]")
      "$effekt.handle" <> cs <> parens(nest(line <> toDoc(body)))
    case Match(sc, clauses) =>
      // TODO using the unqualified name here might lead to wrong operational behavior
      val cs = braces(nest(line <> vsep(clauses map { case (id, b) => toDoc(id.name) <> ":" <+> toDoc(b) }, comma)) <> line)
      "$effekt.match" <> parens(toDoc(sc) <> comma <+> cs)
  }

  def toDocStmt(s: Stmt): Doc = s match {
    case Def(id, BlockDef(ps, body), rest) =>
      "function" <+> toDoc(id.name) <> parens(hsep(ps map toDoc, comma)) <+>
        braces(nest(line <> toDocStmt(body)) <> line) <> emptyline<> toDocStmt(rest)

    case Def(id, Extern(ps, body), rest) =>
      "function" <+> toDoc(id.name) <> parens(hsep(ps map toDoc, comma)) <+>
        braces(nest(line <> "return" <+> body) <> line) <> emptyline <> toDocStmt(rest)

    case Data(did, ctors, rest) =>
      val cs = ctors.map { id =>
        val datastr = "\"" + did.name + "\""
        val consstr = "\"" + id.name + "\""
        "const" <+> toDoc(id.name) <+> "=" <+> "$effekt.constructor" <> parens(datastr <> comma <+> consstr)
      }
      vsep(cs, ";") <> ";" <> line <> line <> toDocStmt(rest)

    case Include(contents, rest) =>
      line <> vsep(contents.split('\n').map(text)) <> emptyline <> toDocStmt(rest)

    case other => "return" <+> toDocExpr(other)
  }

  def requiresBlock(s: Stmt): Boolean = s match {
    case Data(did, ctors, rest) => true
    case Def(id, d, rest) => true
    case _ => false
  }

}