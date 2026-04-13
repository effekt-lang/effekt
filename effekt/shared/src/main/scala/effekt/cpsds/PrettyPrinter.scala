package effekt
package cpsds

import core.Id
import kiama.output.ParenPrettyPrinter
import effekt.source.FeatureFlag

object PrettyPrinter extends ParenPrettyPrinter {

  import kiama.output.PrettyPrinterTypes.Document

  override val defaultIndent = 2

  def format(t: ModuleDecl): Document =
    pretty(toDoc(t), 4)

  def format(defs: List[ToplevelDefinition]): String =
    pretty(toDoc(defs), 60).layout

  def format(s: Stmt): String =
    pretty(toDoc(s), 60).layout

  def format(e: Expr): String =
    pretty(toDoc(e), 60).layout

  val show: PartialFunction[Any, String] = {
    case m: ModuleDecl => format(m).layout
    case d: ToplevelDefinition => format(List(d))
    case s: Stmt => format(s)
    case e: Expr => format(e)
    case x: Id => x.show
  }

  val emptyline: Doc = line <> line

  def toDoc(m: ModuleDecl): Doc = {
    "module" <+> m.path <> emptyline <>
    toDoc(m.definitions)
  }

  def toDoc(definitions: List[ToplevelDefinition]): Doc =
    vsep(definitions map toDoc, semi)

  def toDoc(d: ToplevelDefinition): Doc = d match {
    case ToplevelDefinition.Def(id, params, body) =>
      "def" <+> toDoc(id) <> parens(params.map(toDoc)) <+> "=" <+> nested(toDoc(body))
    case ToplevelDefinition.Val(id, ks, k, binding) =>
      "let" <+> toDoc(id) <+> "|" <+> toDoc(ks) <> "," <+> toDoc(k) <+> "=" <+> toDoc(binding)
    case ToplevelDefinition.Let(id, binding) =>
      "let" <+> toDoc(id) <+> "=" <+> toDoc(binding)
  }

  def toDoc(e: Expr): Doc = e match {
    case Expr.Variable(id) => toDoc(id)
    case Expr.Literal((), core.Type.TUnit) => "()"
    case Expr.Literal(s: String, core.Type.TString) => "\"" + s + "\""
    case Expr.Literal(value, _) => value.toString
    case Expr.Make(data, tag, vargs) => toDoc(tag) <> parens(vargs.map(toDoc))
    case Expr.Abort => "abort"
    case Expr.Return => "return"
    case Expr.Toplevel => "toplevel"
  }

  def toDoc(s: Stmt): Doc = s match {
    case Stmt.Def(id, params, body, rest) =>
      "def" <+> toDoc(id) <> parens(params.map(toDoc)) <+> "=" <+> nested(toDoc(body)) <> line <>
        toDoc(rest)

    case Stmt.New(id, interface, operations, rest) =>
      "new" <+> toDoc(id) <+> ":" <+> toDoc(interface.name) <+> block(operations.map(toDoc)) <> line <>
        toDoc(rest)

    case Stmt.Let(id, binding, rest) =>
      "let" <+> toDoc(id) <+> "=" <+> toDoc(binding) <> line <>
        toDoc(rest)

    case Stmt.App(id, args) =>
      toDoc(id) <> parens(args.map(toDoc))

    case Stmt.Invoke(id, method, args) =>
      toDoc(id) <> "." <> method.name.toString <> parens(args.map(toDoc))

    case Stmt.Run(id, callee, args, purity, rest) =>
      val prefix = purity match {
        case Purity.Pure => "run"
        case Purity.Impure => "run!"
        case Purity.Async => "run~"
      }
      prefix <+> toDoc(id) <+> "=" <+> toDoc(callee) <> parens(args.map(toDoc)) <> line <>
        toDoc(rest)

    case Stmt.If(cond, thn, els) =>
      "if" <+> parens(toDoc(cond)) <+> block(toDoc(thn)) <+> "else" <+> block(toDoc(els))

    case Stmt.Match(scrutinee, clauses, default) =>
      val cs = braces(nest(line <> vsep(clauses map { case (p, c) =>
        "case" <+> toDoc(p) <+> toDoc(c)
      })) <> line)
      val d = default.map { body =>
        space <> "else" <+> block(toDoc(body))
      }.getOrElse(emptyDoc)
      toDoc(scrutinee) <+> "match" <+> cs <> d

    case Stmt.Region(id, ks, rest) =>
      "region" <+> toDoc(id) <+> "@" <+> toDoc(ks) <+> block(toDoc(rest))

    case Stmt.Alloc(id, init, region, rest) =>
      "alloc" <+> toDoc(id) <+> "in" <+> toDoc(region) <+> "=" <+> toDoc(init) <> ";" <> line <>
        toDoc(rest)

    case Stmt.Var(id, init, ks, rest) =>
      "var" <+> toDoc(id) <+> "=" <+> toDoc(init) <+> "@" <+> toDoc(ks) <> ";" <> line <>
        toDoc(rest)

    case Stmt.Dealloc(ref, rest) =>
      "dealloc" <> parens(toDoc(ref)) <> ";" <> line <>
        toDoc(rest)

    case Stmt.Get(ref, id, rest) =>
      "let" <+> toDoc(id) <+> "=" <+> "!" <> toDoc(ref) <> ";" <> line <>
        toDoc(rest)

    case Stmt.Put(ref, value, rest) =>
      toDoc(ref) <+> ":=" <+> toDoc(value) <> ";" <> line <>
        toDoc(rest)

    case Stmt.Reset(p, ks, k, body, ks1, k1) =>
      "reset" <> parens(toDoc(p) <> "," <+> toDoc(ks) <> "," <+> toDoc(k)) <+>
        block(toDoc(body)) <+> "@" <+> toDoc(ks1) <> "," <+> toDoc(k1)

    case Stmt.Shift(prompt, resume, ks, k, body, ks1, k1) =>
      "shift" <> parens(toDoc(prompt)) <+>
        braces(space <> toDoc(resume) <> "," <+> toDoc(ks) <> "," <+> toDoc(k) <+> "=>" <+>
          nest(line <> toDoc(body)) <> line) <+>
        "@" <+> toDoc(ks1) <> "," <+> toDoc(k1)

    case Stmt.Resume(r, ks, k, body, ks1, k1) =>
      "resume" <> parens(toDoc(r)) <+>
        braces(space <> toDoc(ks) <> "," <+> toDoc(k) <+> "=>" <+>
          nest(line <> toDoc(body)) <> line) <+>
        "@" <+> toDoc(ks1) <> "," <+> toDoc(k1)

    case Stmt.Hole(span) =>
      "<>" <+> s"// @ ${span.range.from.format}"
  }

  def toDoc(clause: Clause): Doc = clause match {
    case Clause(params, body) =>
      parens(params.map(toDoc)) <+> "=>" <+> block(toDoc(body))
  }

  def toDoc(op: Operation): Doc = op match {
    case Operation(name, params, body) =>
      "def" <+> toDoc(name) <> parens(params.map(toDoc)) <+> "=" <+> nested(toDoc(body))
  }

  def toDoc(s: symbols.Symbol): Doc = s.show

  def nested(content: Doc): Doc = group(nest(line <> content))

  def parens(docs: List[Doc]): Doc = parens(hsep(docs, comma))

  def braces(docs: List[Doc]): Doc = braces(hsep(docs, semi))

  def block(content: Doc): Doc = braces(nest(line <> content) <> line)

  def block(docs: List[Doc]): Doc = block(vsep(docs, line))
}
