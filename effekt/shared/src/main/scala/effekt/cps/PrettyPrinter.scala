package effekt
package cps

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

  def format(t: Block): String =
    pretty(toDoc(t), 60).layout

  def format(e: Expr): String =
    pretty(toDoc(e), 60).layout

  val show: PartialFunction[Any, String] = {
    case m: ModuleDecl => format(m).layout
    case d: ToplevelDefinition => format(List(d))
    case s: Stmt => format(s)
    case b: Block => format(b)
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
    case ToplevelDefinition.Def(id, block) =>
      "def" <+> toDoc(id) <+> "=" <+> toDoc(block)
    case ToplevelDefinition.Val(id, ks, k, binding) =>
      "let" <+> toDoc(id) <+> "|" <+> toDoc(ks) <> "," <+> toDoc(k) <+> "=" <+> toDoc(binding)
    case ToplevelDefinition.Let(id, binding) =>
      "let" <+> toDoc(id) <+> "=" <+> toDoc(binding)
  }

  def toDoc(e: Expr): Doc = e match {
    case Expr.ValueVar(id) => toDoc(id)
    case Expr.Literal(()) => "()"
    case Expr.Literal(s: String) => "\"" + s + "\""
    case Expr.Literal(value) => value.toString
    case Expr.PureApp(id, vargs) => toDoc(id) <> argsToDoc(vargs, Nil)
    case Expr.Make(data, tag, vargs) => "make" <+> toDoc(data.name) <+> toDoc(tag) <> argsToDoc(vargs, Nil)
    case Expr.Box(b) => parens("box" <+> toDoc(b))
  }

  def toDoc(b: Block): Doc = b match {
    case Block.BlockVar(id) => toDoc(id)
    case b: Block.BlockLit => toDoc(b)
    case Block.Unbox(e) => parens("unbox" <+> toDoc(e))
    case Block.New(handler) => "new" <+> toDoc(handler)
  }

  def toDoc(b: Block.BlockLit): Doc = b match {
    case Block.BlockLit(vps, bps, ks, k, body) =>
      val params = if (vps.isEmpty && bps.isEmpty) emptyDoc else
        parens(hsep(vps.map(toDoc), comma)) <+> hsep(bps.map(toDoc))
      braces {
        space <> params <+> "|" <+> toDoc(ks) <> "," <+> toDoc(k) <+> "=>" <+>
        nest(line <> toDoc(body)) <> line
      }
  }

  def toDoc(s: Stmt): Doc = s match {
    case Stmt.Jump(k, vargs, ks) =>
      "jump" <+> toDoc(k) <> argsToDoc(vargs, Nil) <+> "@" <+> toDoc(ks)

    case Stmt.App(callee, vargs, bargs, ks, k) =>
      toDoc(callee) <> argsToDoc(vargs, bargs) <+> "@" <+> toDoc(ks) <> "," <+> toDoc(k)

    case Stmt.Invoke(callee, method, vargs, bargs, ks, k) =>
      toDoc(callee) <> "." <> method.name.toString <> argsToDoc(vargs, bargs) <+> "@" <+> toDoc(ks) <> "," <+> toDoc(k)

    case Stmt.If(cond, thn, els) =>
      "if" <+> parens(toDoc(cond)) <+> block(toDoc(thn)) <+> "else" <+> block(toDoc(els))

    case Stmt.Match(scrutinee, clauses, default) =>
      val cs = braces(nest(line <> vsep(clauses map { case (p, b) =>
        "case" <+> toDoc(p) <+> toDoc(b)
      })) <> line)
      val d = default.map { body =>
        space <> "else" <+> block(toDoc(body))
      }.getOrElse(emptyDoc)
      toDoc(scrutinee) <+> "match" <+> cs <> d

    case Stmt.LetDef(id, binding, body) =>
      "def" <+> toDoc(id) <+> "=" <+> toDoc(binding) <> line <>
        toDoc(body)

    case Stmt.LetExpr(id, binding, body) =>
      "let" <+> toDoc(id) <+> "=" <+> toDoc(binding) <> line <>
        toDoc(body)

    case Stmt.LetCont(id, binding, body) =>
      "let" <+> toDoc(id) <+> "=" <+> toDoc(binding) <> line <>
        toDoc(body)

    case Stmt.DirectApp(id, callee, vargs, bargs, body) =>
      "let!" <+> toDoc(id) <+> "=" <+> toDoc(callee) <> argsToDoc(vargs, bargs) <> line <>
        toDoc(body)

    case Stmt.Region(id, ks, body) =>
      "region" <+> toDoc(id) <+> "@" <+> toDoc(ks) <+> block(toDoc(body))

    case Stmt.Alloc(id, init, region, body) =>
      "var" <+> toDoc(id) <+> "in" <+> toDoc(region) <+> "=" <+> toDoc(init) <> ";" <> line <>
        toDoc(body)

    case Stmt.Dealloc(ref, body) =>
      "dealloc" <> parens(toDoc(ref)) <> ";" <> line <>
        toDoc(body)

    case Stmt.Var(id, init, ks, body) =>
      "var" <+> toDoc(id) <+> "=" <+> toDoc(init) <+> "@" <+> toDoc(ks) <> ";" <> line <>
        toDoc(body)

    case Stmt.Get(ref, id, body) =>
      "let" <+> toDoc(id) <+> "=" <+> "!" <> toDoc(ref) <> ";" <> line <>
        toDoc(body)

    case Stmt.Put(ref, value, body) =>
      toDoc(ref) <+> ":=" <+> toDoc(value) <> ";" <> line <>
        toDoc(body)

    case Stmt.Reset(prog, ks, k) =>
      "reset" <+> toDoc(prog) <+> "@" <+> toDoc(ks) <> "," <+> toDoc(k)

    case Stmt.Shift(prompt, body, ks, k) =>
      "shift" <> parens(toDoc(prompt)) <+> toDoc(body) <+> "@" <+> toDoc(ks) <> "," <+> toDoc(k)

    case Stmt.Resume(r, body, ks, k) =>
      "resume" <> parens(toDoc(r)) <+> toDoc(body) <+> "@" <+> toDoc(ks) <> "," <+> toDoc(k)

    case Stmt.Hole(span) =>
      "<>" <+> s"// @ ${span.range.from.format}"
  }

  def toDoc(clause: Clause): Doc = clause match {
    case Clause(vparams, body) =>
      parens(hsep(vparams.map(toDoc), comma)) <+> "=>" <+> block(toDoc(body))
  }

  def toDoc(impl: Implementation): Doc = {
    val handlerName = toDoc(impl.interface.name)
    val clauses = impl.operations.map { op =>
      "def" <+> toDoc(op.name) <> paramsToDoc(op.vparams, op.bparams, op.ks, op.k) <+> "=" <+>
      nested(toDoc(op.body))
    }
    handlerName <+> block(vsep(clauses))
  }

  def toDoc(k: Cont): Doc = k match {
    case Cont.ContVar(id) => toDoc(id)
    case Cont.ContLam(results, ks, body) =>
      braces {
        space <> parens(hsep(results.map(toDoc), comma)) <+> "|" <+> toDoc(ks) <+> "=>" <+>
        nest(line <> toDoc(body)) <> line
      }
    case Cont.Abort => "abort"
  }

  def toDoc(ks: MetaCont): Doc = toDoc(ks.id)

  def toDoc(s: symbols.Symbol): Doc = s.show

  def argsToDoc(vargs: List[Expr], bargs: List[Block]): Doc = {
    val vargsDoc = if (vargs.isEmpty && !bargs.isEmpty) emptyDoc else parens(vargs.map(toDoc))
    val bargsDoc = if (bargs.isEmpty) emptyDoc else hcat(bargs.map { b => braces(toDoc(b)) })
    vargsDoc <> bargsDoc
  }

  def paramsToDoc(vps: List[Id], bps: List[Id], ks: Id, k: Id): Doc = {
    val vpsDoc = if (vps.isEmpty && !bps.isEmpty) emptyDoc else parens(vps.map(toDoc))
    val bpsDoc = if (bps.isEmpty) emptyDoc else hcat(bps.map(toDoc))
    vpsDoc <> bpsDoc <+> "|" <+> toDoc(ks) <> "," <+> toDoc(k)
  }

  def nested(content: Doc): Doc = group(nest(line <> content))

  def parens(docs: List[Doc]): Doc = parens(hsep(docs, comma))

  def braces(docs: List[Doc]): Doc = braces(hsep(docs, semi))

  def block(content: Doc): Doc = braces(nest(line <> content) <> line)

  def block(docs: List[Doc]): Doc = block(vsep(docs, line))
}
