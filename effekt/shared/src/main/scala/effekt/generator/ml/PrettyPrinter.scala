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
    toDoc(bindings) <> line <> line <> toDoc(body) <> ";" <> line
  }

  def toDoc(bindings: List[Binding]): Doc = {
    vsep(bindings map toDoc, line <> line)
  }

  def format(doc: Doc): Document =
    pretty(doc)

  def toDoc(binding: Binding): Doc = binding match {
    case ValBind(name, body) =>
      "val" <+> toDoc(name) <+>
        "=" <> nest(line <> toDoc(body) <> ";")
    case FunBind(name, params, body) =>
      "fun" <+> toDoc(name) <+> hsep(params map toDoc) <+>
        "=" <> nest(line <> toDoc(body) <> ";")
    case RawBind(raw) =>
      string(raw)
  }

  def toDoc(expr: Expr): Doc = expr match {
    case Expr.Call(callee, args) =>
      val call = toDoc(callee) <+> hsep(args map toDoc)
      parens(call)
    case Expr.RawValue(raw) =>
      string(raw)
    case Expr.RawExpr(raw) =>
      val expr = string(raw)
      parens(expr)
    case Expr.Let(bindings, body) =>
      val let =
        "let" <> nest(line <>
          vsep(bindings map toDoc)
        ) <> line <>
        "in" <> nest(line <>
          toDoc(body)
        ) <> line <>
        "end"
      parens(let)
    case Expr.Lambda(params, body) =>
      val lambda = "fn" <+> hsep(params map toDoc) <+> "=>" <+> toDoc(body)
      parens(lambda)
    case Expr.If(cond, thn, els) =>
      val ifs = "if" <+> toDoc(cond) <+> "then" <+> toDoc(thn) <+> "else" <+> toDoc(els)
      parens(ifs)
    case Expr.Variable(name) =>
      toDoc(name)
    case Expr.Sequence(head, rest) =>
      val seq = toDoc(head) <> ";" <@> toDoc(rest)
      parens(seq)
  }

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
