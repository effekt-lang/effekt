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
    case DataBind(name, tparams, constructors) =>
      val args: Doc = if (tparams.isEmpty) "" else parens(hsep(tparams map toDoc, ","))
      "datatype" <+> args <+> toDoc(name) <+> "=" <> nest(line <>
        ssep(constructors.map {
          case (name, None) => toDoc(name)
          case (name, Some(tpe)) => toDoc(name) <+> "of" <+> toDoc(tpe)
        }, line <> "| ") <>
        ";"
      )
    case RawBind(raw) =>
      string(raw)
  }

  def toDoc(tpe: ml.Type): Doc = tpe match {
    case Type.Var(n) => toDoc(n)
    case Type.Tuple(Nil) => ???
    case Type.Tuple(t :: Nil) => toDoc(t)
    case Type.Tuple(l) =>
      val d = ssep(l map toDoc, " * ")
      parens(d)
    case Type.Integer => "int"
    case Type.Real => "real"
    case Type.String => "string"
    case Type.Bool => "bool"
    case Type.Data(name) => toDoc(name)
    case Type.Tapp(tpe, arg) =>
      val ap = toDoc(arg) <+> toDoc(tpe)
      parens(ap)
    case Type.Builtin(t) => toDoc(t)
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
      "{" <> hsep(fieldAssignments, ", ") <> "}"
    case Expr.Sequence(exps, rest) =>
      val seq = vsep(exps map toDoc, "; ") <> ";" <@> toDoc(rest)
      parens(seq)
    case Expr.Match(scrutinee, clauses, default) =>
      val mlDefault: Doc = default match {
        case None => ""
        case Some(d) =>
          val delim: Doc = if (clauses.isEmpty) "" else line <> "| "
          delim <> "_ =>" <+> toDoc(d)
      }
      val mlMatch = "case" <+> toDoc(scrutinee) <+> "of" <+> nest(line <>
        ssep(clauses map toDoc, line <> "| ") <> mlDefault
      ) <> line
      parens(mlMatch)
  }

  def toDoc(mc: ml.MatchClause): Doc = {
    toDoc(mc.pattern) <+> "=>" <+> nest(line <> toDoc(mc.body))
  }

  def toDoc(p: ml.Pattern): Doc = p match {
    case Pattern.Record(assignments) =>
      "{" <>
        hsep(assignments map { case (fname, binder) => toDoc(fname) <+> "=" <+> toDoc(binder) }, ",") <>
        "}"
    case Pattern.Datatype(tag, terms) if terms.isEmpty =>
      toDoc(tag)
    case Pattern.Datatype(tag, terms) =>
      toDoc(tag) <> parens(hsep(terms map toDoc, ","))
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
