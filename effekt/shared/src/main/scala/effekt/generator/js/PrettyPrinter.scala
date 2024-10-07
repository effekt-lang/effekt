package effekt
package generator
package js

import effekt.util.intercalate
import kiama.output.ParenPrettyPrinter
import kiama.output.PrettyPrinterTypes.Document

import scala.language.implicitConversions

object PrettyPrinter extends ParenPrettyPrinter {

  override val defaultIndent = 2

  def toDoc(name: JSName): Doc = name.name

  def format(stmts: List[Stmt]): Document =
    pretty(vsep(stmts map toDoc, line))

  val show: PartialFunction[Any, String] = {
    case m: js.Module  => format(m.stmts).layout
  }

  def toDoc(expr: Expr): Doc = expr match {
    case Call(callee, args)           => toDocParens(callee) <> parens(args map toDoc)
    case New(callee, args)            => "new" <+> toDocParens(callee) <> parens(args map toDoc)
    case RawExpr(strings, args)       => hcat(intercalate(strings.map(string), args.map(toDocAsAtom)))
    case RawLiteral(content)          => string(content)
    case Member(callee, selection)    => toDocParens(callee) <> "." <> toDoc(selection)
    case IfExpr(cond, thn, els)       => parens(toDoc(cond)) <+> "?" <+> toDoc(thn) <+> ":" <+> toDoc(els)
    case Lambda(params, Return(expr)) => parens(params map toDoc) <+> "=>" <> nested(toDoc(expr))
    case Lambda(params, Block(stmts)) => parens(params map toDoc) <+> "=>" <+> jsBlock(stmts.map(toDoc))
    case Lambda(params, body)         => parens(params map toDoc) <+> "=>" <> jsBlock(toDoc(body))
    case Object(properties)           => group(jsBlock(vsep(properties.map { case (n, d) => toDoc(n) <> ":" <+> toDoc(d) }, comma)))
    case ArrayLiteral(elements)       => brackets(elements map toDoc)
    case Variable(name)               => toDoc(name)
  }

  // to be used in low precedence positions
  def toDocParens(e: Expr): Doc = e match {
    case e: IfExpr => parens(toDoc(e))
    case e: Lambda => parens(toDoc(e))
    case e => toDoc(e)
  }

  // to be used in really low precedence positions
  def toDocAsAtom(e: Expr): Doc = e match {
    case e: Variable => toDoc(e)
    case e: ArrayLiteral => toDoc(e)
    case e: RawLiteral => toDoc(e)
    case e => parens(toDoc(e))
  }

  def toDoc(stmt: Stmt): Doc = stmt match {
    case RawStmt(raw)                  => string(raw) //  vsep(raw.split('\n').toList.map(c => text(c)))
    case Block(stmts)                  => jsBlock(stmts map toDoc)
    case Return(expr)                  => "return" <+> toDoc(expr) <> ";"
    case ExprStmt(expr)                => toDoc(expr) <> ";"
    case Const(id, expr)               => "const" <+> toDoc(id) <+> "=" <+> toDoc(expr) <> ";"
    case Let(id, expr)                 => "let" <+> toDoc(id) <+> "=" <+> toDoc(expr) <> ";"
    case Destruct(ids, expr)           => "const" <+> braces(hsep(ids.map(toDoc), comma)) <+> "=" <+> toDoc(expr) <> ";"
    case Assign(target, expr)          => toDoc(target) <+> "=" <+> toDoc(expr) <> ";"
    case Function(name, params, stmts) => "function" <+> toDoc(name) <> parens(params map toDoc) <+> jsBlock(stmts map toDoc)
    case Class(name, methods)          => "class" <+> toDoc(name) <+> jsBlock(methods.map(jsMethod))
    case If(cond, thn, Block(Nil))     => "if" <+> parens(toDoc(cond)) <+> toDoc(thn)
    case If(cond, thn, els)            => "if" <+> parens(toDoc(cond)) <+> toDoc(thn) <+> "else" <+> toDoc(els)
    case Try(prog, id, handler, Nil)   => "try" <+> jsBlock(prog.map(toDoc)) <+> "catch" <+> parens(toDoc(id)) <+> jsBlock(handler.map(toDoc))
    case Try(prog, id, handler, fin)    => "try" <+> jsBlock(prog.map(toDoc)) <+> "catch" <+> parens(toDoc(id)) <+> jsBlock(handler.map(toDoc)) <+> "finally" <+> jsBlock(fin.map(toDoc))
    case Throw(expr)                   => "throw" <+> toDoc(expr) <> ";"
    case Break()                       => "break;"
    case Continue(label)               => "continue" <> label.map(l => space <> toDoc(l)).getOrElse(emptyDoc) <> ";"
    case While(cond, stmts, label)     =>
      label.map(l => toDoc(l) <> ":" <> space).getOrElse(emptyDoc) <>
        "while" <+> parens(toDoc(cond)) <+> jsBlock(stmts.map(toDoc))

    case Switch(sc, branches, default) => "switch" <+> parens(toDoc(sc)) <+> jsBlock(branches.map {
      case (tag, stmts) => "case" <+> toDoc(tag) <> ":" <+> nested(stmts map toDoc)
    } ++ default.toList.map { stmts => "default:" <+> nested(stmts map toDoc) })
  }

  def jsMethod(c: js.Function): Doc = c match {
    case js.Function(name, params, stmts) =>
      toDoc(name) <> parens(params map toDoc) <+> jsBlock(stmts.map(toDoc))
  }

  def toDoc(pattern: Pattern): Doc = pattern match {
    case Pattern.Variable(name) => toDoc(name)
    case Pattern.Array(ps) => brackets(ps map toDoc)
  }

  // some helpers

  val emptyline: Doc = line <> line

  def nested(content: Doc): Doc = group(nest(line <> content))

  def nested(docs: List[Doc]): Doc = group(nest(line <> vcat(docs)))

  def parens(docs: List[Doc]): Doc = parens(hsep(docs, comma))

  def brackets(docs: List[Doc]): Doc = brackets(hsep(docs, comma))

  def jsBlock(content: Doc): Doc = braces(nest(line <> content) <> line)

  def jsBlock(docs: List[Doc]): Doc = jsBlock(vcat(docs))
}
