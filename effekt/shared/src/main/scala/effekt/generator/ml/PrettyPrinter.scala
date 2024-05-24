package effekt
package generator
package ml

import effekt.util.intercalate
import kiama.output.ParenPrettyPrinter
import kiama.output.PrettyPrinterTypes.Document

import scala.language.implicitConversions

object PrettyPrinter extends ParenPrettyPrinter {

  override val defaultIndent = 2

  //val prelude = "#!/usr/local/bin/scheme --script\n\n(import (chezscheme))\n\n"

  def toDoc(name: MLName): Doc = text(name.name)

  def toDoc(toplevel: Toplevel): Doc = {
    val Toplevel(bindings, body) = toplevel
    toDoc(bindings) <> line <> line <>
      "let" <+> "_" <+> "=" <+> toDoc(body) <> line
  }

  def toDoc(bindings: List[Binding]): Doc = {
    vsep(bindings map toDoc, line <> line)
  }

  def format(doc: Doc): Document =
    pretty(doc)

  def toDoc(binding: Binding): Doc = binding match {
    case AnonBind(body) =>
      group("let" <+> "_"         <+> "=" <> nest(line <> toDoc(body)))
    case ValBind(name, body) =>
      group("let" <+> toDoc(name) <+> "=" <> nest(line <> toDoc(body)))
    case FunBind(name, params, body) =>
      group("let" <+> "rec" <+> toDoc(name) <+> argList(params, paramToDoc, true) <+>
        "=" <> nest(line <> toDoc(body)))
    case DataBind(name, tparams, constructors) =>
      "type" <+> tlistDoc(tparams) <+> toDoc(name) <+> "=" <> nest(line <>
        vcat(constructors.map {
          case (name, None) => "|" <+> toDoc(name)
          case (name, Some(tpe)) => "|" <+> toDoc(name) <+> "of" <+> toDoc(tpe)
        }))
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

  def paramToDoc(p: ml.Param): Doc = p match {
    case ml.Param.Named(pname) => toDoc(pname)
    case ml.Param.Patterned(p) => parens(toDoc(p))
  }

  def toDoc(tpe: ml.Type): Doc = tpe match {
    case Type.Var(n) => "'" <> toDoc(n)
    case Type.Tuple(Nil) => ???
    case Type.Tuple(t :: Nil) => ???
    case Type.Tuple(l) =>
      val d = ssep(l map toDoc, " * ")
      parens(d)
    case Type.Unit => "unit"
    case Type.Integer => "int"
    case Type.Real => "float"
    case Type.String => "string"
    case Type.Bool => "bool"
    case Type.Data(name) => toDoc(name)
    case Type.TApp(tpe, args) =>
      val ap = tlistDoc(args) <+> toDoc(tpe)
      parens(ap)
    case Type.Builtin(t) => toDoc(t)
    case Type.Fun(Nil, res) => ???
    case Type.Fun(args, res) =>
      val fn = folddoc(args.map(a => toDoc(a) <+> "-> "), _ <> _) <+> toDoc(res)
      parens(fn)
  }

  def toDoc(expr: Expr): Doc = expr match {
    case Expr.Call(callee, args) =>
      parens(group(nest(toDoc(callee) <@> argList(args, toDoc))))
    case Expr.RawValue(raw) => string(raw)
    case Expr.RawExpr(strings, args) => hcat(intercalate(strings.map(string), args.map(toDoc)))
    case Expr.Let(binding :: Nil, body) =>
      val let =
        group(toDoc(binding)) <>
          line <>
          "in" <+> group(toDoc(body)) <> line
      let
    case Expr.Let(bindings, body) =>
      val let =
        nest(line <>
          vcat(bindings map { b => toDoc(b) <+> "in" })
        ) <> line <>
          group(nest(line <>
          toDoc(body)
        )) <> line
      let
    case Expr.Lambda(Nil, body) =>
      val lambda = "fun () ->" <+> toDoc(body)
      parens(lambda)
    case Expr.Lambda(params, body) =>
      val paramDocs = params.map{p => "fun" <+> paramToDoc(p) <+> "->"}
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
    case Expr.Make(tag, None) =>
        toDoc(tag)
    case Expr.Make(tag, Some(arg)) =>
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
          delim <> "_ ->" <+> toDoc(d)
      }
      val mlMatch = group("match" <@> toDoc(scrutinee) <@> "with") <+> nest(line <>
        ssep(clauses map { c => "|" <+> toDoc(c) }, line) <> mlDefault
      )
      group(nest("(" <@> mlMatch <@> ")"))
    case Expr.Ref(exp) =>
      val ref = "ref" <+> toDoc(exp)
      parens(ref)
    case Expr.Deref(exp) =>
      val mlDeref = "!" <> toDoc(exp)
      parens(mlDeref)
    case Expr.Assign(asignee, value) =>
      val assign = toDoc(asignee) <+> ":=" <+> toDoc(value)
      parens(assign)
  }

  def toDoc(mc: ml.MatchClause): Doc = {
    toDoc(mc.pattern) <+> "->" <+> toDoc(mc.body)
  }

  def toDoc(p: ml.Pattern): Doc = p match {
    case Pattern.Wild() => "_"
    case Pattern.Named(n) => toDoc(n)
    case Pattern.Datatype(tag, Nil) =>
      toDoc(tag)
    case Pattern.Datatype(tag, one :: Nil) =>
      toDoc(tag) <+> toDoc(one)
    case Pattern.Datatype(tag, terms) =>
      toDoc(tag) <+> parens(hsep(terms map toDoc, ","))
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
