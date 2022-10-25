package effekt
package generator
package ml

import kiama.output.ParenPrettyPrinter
import kiama.output.PrettyPrinterTypes.Document

import scala.language.implicitConversions

object PrettyPrinter extends ParenPrettyPrinter {

  override val defaultIndent = 2

  //val prelude = "#!/usr/local/bin/scheme --script\n\n(import (chezscheme))\n\n"

  def toDoc(name: MLName): Doc = text(name.name)

  def toDoc(toplevel: Toplevel): Doc = {
    val Toplevel(bindings, body) = toplevel
    vsep(bindings map toDoc, line) <> line <> line <> toDoc(body) <> ";"
  }

  def format(defs: List[Binding]): Document =
    pretty(vsep(defs map toDoc, line <> line))

  def toDoc(binding: Binding): Doc = binding match {
    case ValBind(name, body) =>
      "val" <+> toDoc(name) <+>
        "=" <> nest(line <> toDoc(body) <> ";")
    case FunBind(name, params, body) =>
      "fun" <+> toDoc(name) <> argList(params map toDoc) <+>
        "=" <> nest(line <> toDoc(body) <> ";")
    case RawBind(raw) =>
      string(raw)
  }

  def toDoc(expr: Expr): Doc = expr match {
    case Expr.Call(callee, args) =>
      toDoc(callee) <> argList(args map toDoc)
    case Expr.RawValue(raw) =>
      string(raw)
    case Expr.RawExpr(raw) =>
      string(raw)
    case Expr.Let(bindings, body) =>
      "let" <> nest(line <>
        vsep(bindings map toDoc)
      ) <> line <>
      "in" <> nest(line <>
        toDoc(body)
      ) <> line <>
      "end"
    case Expr.Lambda(params, body) =>
      "fn" <+> argList(params map toDoc) <+> "=>" <+> toDoc(body)
    case Expr.If(cond, thn, els) =>
      "if" <+> toDoc(cond) <+> "then" <+> toDoc(thn) <+> "else" <+> toDoc(els)
    case Expr.Variable(name) => toDoc(name)
  }

  def argList(args: Seq[Doc]): Doc = arguments(args, identity)
//    expr match {
//    case Call(callee, Nil)       => parens(toDoc(callee))
//    case Call(callee, arguments) => parens(toDoc(callee) <+> group(align(hsep(arguments map toDoc, line))))
//    case RawExpr(raw)            => string(raw)
//    case RawValue(raw)           => string(raw)
//    case Let(Nil, body)          => toDoc(body)
//    case Let(bindings, body)     => parens("let" <+> align(vcat(bindings map toDoc)) <+> "in" <+> toDoc(body) <+> "end")
//    case Let_*(bindings, body)   => parens("let*" <+> parens(align(vcat(bindings map toDoc))) <> toDoc(body))
//    case Lambda(param :: Nil, body) => parens("fn" <+> toDoc(param) <+> "=>" <+> toDoc(body))
//    case Lambda(params, body)    => parens("fn" <+> arguments(params map toDoc) <+> "=>" <+> toDoc(body))
//    case If(cond, thn, els)      => parens("if" <+> toDoc(cond) <+> "then" <+> nest(line <> toDoc(thn)) <+> "else" <+> nest(line <> toDoc(els)))
//    case Variable(name)          => toDoc(name)
//    case Match(sc, clauses) => parens("pattern-match" <+> toDoc(sc) <> nest(line <> parens(align(vcat(clauses.map {
//      case (pattern, branch) => brackets(toDoc(pattern) <+> toDoc(branch))
//    })))))
//    case Handle(handlers, body) => parens("handle" <+> parens(align(vcat(handlers map toDoc))) <+> nest(line <> toDoc(body)))
//  }
}
