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
      "fun" <+> toDoc(name) <+> argList(params, toDoc) <+>
        "=" <> nest(line <> toDoc(body) <> ";")
    case RawBind(raw) =>
      string(raw)
  }

  def toDoc(expr: Expr): Doc = expr match {
    case Expr.Call(callee, args) =>
      val call = toDoc(callee) <+> argList(args, toDoc)
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
      val lambda = "fn" <+> argList(params, toDoc) <+> "=>" <+> toDoc(body)
      parens(lambda)
    case Expr.If(cond, thn, els) =>
      val ifs = "if" <+> toDoc(cond) <+> "then" <+> toDoc(thn) <+> "else" <+> toDoc(els)
      parens(ifs)
    case Expr.Variable(name) =>
      toDoc(name)
    case Expr.FieldLookup(record, field) =>
      val lookup = "#" <> toDoc(field) <+> toDoc(record)
      parens(lookup)
    case Expr.MakeRecord(fields) =>
      val fieldAssignments = fields.map((name, exp) => toDoc(name) <+> "=" <+> toDoc(exp))
      "{" <> hsep(fieldAssignments, ", ")  <> "}"
    case Expr.Sequence(exps, rest) =>
      val seq = vsep(exps map toDoc, "; ") <> ";" <@> toDoc(rest)
      parens(seq)
  }

  /**
   * Returns `()` for empty lists and `toDoc(a) toDoc(b) ...` otherwise.
   */
  def argList[A](args: Seq[A], toDoc: A => Doc): Doc = {
    if (args.isEmpty) {
      "()"
    } else {
      hsep(args map toDoc)
    }
  }

}
