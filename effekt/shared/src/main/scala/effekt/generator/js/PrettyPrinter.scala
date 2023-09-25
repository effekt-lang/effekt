package effekt
package generator
package js

import kiama.output.ParenPrettyPrinter
import kiama.output.PrettyPrinterTypes.Document

import scala.language.implicitConversions

object PrettyPrinter extends ParenPrettyPrinter {

  def toDoc(name: JSName): Doc = name.name

  def format(stmts: List[Stmt]): Document =
    pretty(vsep(stmts map toDoc, line))

  def toDoc(expr: Expr): Doc = expr match {
    case Call(callee, args)            => toDoc(callee) <> parens(args map toDoc)
    case RawExpr(raw)                  => string(raw)
    case Member(callee, selection)     => toDoc(callee) <> "." <> toDoc(selection)
    case IfExpr(cond, thn, els)        => parens(parens(toDoc(cond)) <+> "?" <+> toDoc(thn) <+> ":" <+> toDoc(els))
    case Lambda(params, Return(exprs)) => exprs match {
        case List(expr) => parens(parens(params map toDoc) <+> "=>" <> nested(toDoc(expr)))
        case _          => parens(parens(params map toDoc) <+> "=>" <> nested(brackets(hsep(exprs.map(toDoc), comma))))
      }
    case Lambda(params, body)          => parens(parens(params map toDoc) <+> "=>" <> nested(toDoc(body)))
    case Object(properties)            => group(jsBlock(vsep(properties.map { case (n, d) => toDoc(n) <> ":" <+> toDoc(d) }, comma)))
    case ArrayLiteral(elements)        => brackets(elements map toDoc)
    case Variable(name)                => toDoc(name)
  }

  def toDoc(stmt: Stmt): Doc = stmt match {
    case RawStmt(raw)                  => string(raw) //  vsep(raw.split('\n').toList.map(c => text(c)))
    case Block(stmts)                  => jsBlock(stmts map toDoc)
    case Return(exprs)                 => exprs match {
      case List(expr) => "return" <+> toDoc(expr) <> ";"
      case _          => "return" <+> brackets(hsep(exprs.map(toDoc), comma)) <> ";"
    }
    case ExprStmt(expr)                => toDoc(expr) <> ";"
    case Const(ids, expr)              =>
      val consts = ids match {
        case List(c) => toDoc(c)
        case cs      => brackets(hsep(cs.map(toDoc), comma))
      }
      "const" <+> consts <+> "=" <+> toDoc(expr) <> ";"
    case Destruct(ids, expr)           => "const" <+> braces(hsep(ids.map(toDoc), comma)) <+> "=" <+> toDoc(expr) <> ";"
    case Assign(target, expr)          => toDoc(target) <+> "=" <+> toDoc(expr) <> ";"
    case Function(name, params, stmts) => "function" <+> toDoc(name) <> parens(params map toDoc) <+> jsBlock(stmts map toDoc)
    case Switch(sc, branches, default) => "switch" <+> parens(toDoc(sc)) <+> jsBlock(branches.map {
      case (tag, body) => "case" <+> toDoc(tag) <> ":" <+> toDoc(body)
    } ++ default.toList.map { body => "default:" <+> toDoc(body) })
  }

  // some helpers

  val emptyline: Doc = line <> line

  def nested(content: Doc): Doc = group(nest(line <> content))

  def parens(docs: List[Doc]): Doc = parens(hsep(docs, comma))

  def brackets(docs: List[Doc]): Doc = brackets(hsep(docs, comma))

  def jsBlock(content: Doc): Doc = braces(nest(line <> content) <> line)

  def jsBlock(docs: List[Doc]): Doc = jsBlock(vsep(docs, line))
}
