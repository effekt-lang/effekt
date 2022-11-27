package effekt
package generator
package ml

import kiama.output.ParenPrettyPrinter
import kiama.output.PrettyPrinterTypes.Document

import scala.language.implicitConversions

/// fillip wadler (prettier pretty printer)
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
    case AnonBind(body) =>
      group("val" <+> "_"         <+> "=" <> nest(line <> toDoc(body) <> ";"))
    case ValBind(name, body) =>
      group("val" <+> toDoc(name) <+> "=" <> nest(line <> toDoc(body) <> ";"))
    case FunBind(name, params, body) =>
      group("fun" <+> toDoc(name) <+> argList(params, toDoc, true) <+>
        "=" <> nest(line <> toDoc(body) <> ";"))
    case DataBind(name, tparams, constructors) =>
      "datatype" <+> tlistDoc(tparams) <+> toDoc(name) <+> "=" <> nest(line <>
        ssep(constructors.map {
          case (name, None) => toDoc(name)
          case (name, Some(tpe)) => toDoc(name) <+> "of" <+> toDoc(tpe)
        }, line <> "| ") <>
        ";"
      )
    case RawBind(raw) =>
      string(raw)
    case TypeBind(name, Nil, tpe) =>
      nest("type" <+> toDoc(name) <+> "=" <@> toDoc(tpe))
    case TypeBind(name, tparams, tpe) =>
      nest("type" <+> tlistDoc(tparams) <+> toDoc(name) <+> "=" <@> toDoc(tpe))
  }

  def tlistDoc(tparams: List[Type]): Doc = tparams match {
    case Nil => ""
    case one :: Nil => toDoc(one)
    case _ => parens(hsep(tparams map toDoc, ","))
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
    case Type.Alias(name) => toDoc(name)
    case Type.Record(fields) =>
      "{" <> hsep(fields.map{case (name, tpe) => toDoc(name) <> ":" <+> toDoc(tpe)}, ",") <> "}"
    case Type.Tapp(tpe, args) =>
      val ap = tlistDoc(args) <+> toDoc(tpe)
      parens(ap)
    case Type.Builtin(t) => toDoc(t)
  }

  def toDoc(expr: Expr): Doc = expr match {
    case Expr.Call(callee, args) =>
      val call = toDoc(callee) <@> argList(args, toDoc)
      parens(group(nest(call)))
    case Expr.RawValue(raw) =>
      string(raw)
    case Expr.RawExpr(raw) =>
      val expr = string(raw)
      parens(expr)
    case Expr.Let(binding :: Nil, body) =>
      val let =
        "let" <+> group(toDoc(binding)) <>
          line <>
          "in" <+> group(toDoc(body)) <> line <>
          "end"
      let
    case Expr.Let(bindings, body) =>
      val let =
        "let" <> nest(line <>
          vsep(bindings map toDoc)
        ) <> line <>
          "in" <> group(nest(line <>
          toDoc(body)
        )) <> line <>
          "end"
      let
    case Expr.Lambda(Nil, body) =>
      val lambda = "fn () =>" <+> toDoc(body)
      parens(lambda)
    case Expr.Lambda(params, body) =>
      val paramDocs = params.map{p => "fn" <+> toDoc(p) <+> "=>"}
      val lambda = nest(hsep(paramDocs) <@> toDoc(body))
      parens(lambda)
    case Expr.If(cond, thn, els) =>
      val ifs = "if" <+> toDoc(cond) <+> "then" <+> toDoc(thn) <+> "else" <+> toDoc(els)
      parens(ifs)
    case Expr.Variable(name) =>
      toDoc(name)
    case Expr.Tuple(Nil) => "()"
    case Expr.Tuple(one :: Nil) => toDoc(one)
    case Expr.Tuple(terms) =>
      parens(hsep(terms map toDoc, ","))
    case Expr.FieldLookup(record, field) =>
      val lookup = "#" <> toDoc(field) <+> toDoc(record)
      parens(lookup)
    case Expr.MakeRecord(fields) =>
      val fieldAssignments = fields.map((name, exp) => toDoc(name) <+> "=" <+> toDoc(exp))
      group(nest("{" <@> vsep(fieldAssignments, ",")) <@> "}")
    case Expr.MakeDatatype(tag, None) =>
        toDoc(tag)
    case Expr.MakeDatatype(tag, Some(arg)) =>
      val mdt = toDoc(tag) <+> toDoc(arg)
      parens(mdt)
    case Expr.Sequence(Nil, rest) => toDoc(rest)
    case Expr.Sequence(exps, rest) =>
      val seq = group(nest("(" <@> vsep((exps :+ rest) map toDoc, ";")) <@> ")")
      seq
    case Expr.Match(scrutinee, clauses, default) =>
      val mlDefault: Doc = default match {
        case None => ""
        case Some(d) =>
          val delim: Doc = if (clauses.isEmpty) "" else line <> "| "
          delim <> "_ =>" <+> toDoc(d)
      }
      val mlMatch = group("case" <@> toDoc(scrutinee) <@> "of") <+> nest(line <>
        ssep(clauses map toDoc, line <> "| ") <> mlDefault
      )
      group(nest("(" <@> mlMatch <@> ")"))
  }

  def toDoc(mc: ml.MatchClause): Doc = {
    toDoc(mc.pattern) <+> "=>" <+> toDoc(mc.body)
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
  def argList[A](args: Seq[A], toDoc: A => Doc, hs: Boolean = false): Doc = {
    if (args.isEmpty) {
      "()"
    } else if (hs) {
      hsep(args map toDoc)
    } else {
      vsep(args map toDoc)
    }
  }

}
