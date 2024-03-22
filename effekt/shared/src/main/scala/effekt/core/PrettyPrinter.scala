package effekt
package core

import kiama.output.ParenPrettyPrinter

import scala.language.implicitConversions
import effekt.symbols.{FeatureFlag, Name, Wildcard, builtins}

object PrettyPrinter extends ParenPrettyPrinter {

  import kiama.output.PrettyPrinterTypes.Document

  override val defaultIndent = 2

  def format(t: ModuleDecl): Document =
    pretty(toDoc(t), 4)

  def format(defs: List[Definition]): String =
    pretty(toDoc(defs), 60).layout

  def format(s: Stmt): String =
    pretty(toDoc(s), 60).layout

  def format(t: ValueType): String =
    pretty(toDoc(t), 60).layout

  def format(t: BlockType): String =
    pretty(toDoc(t), 60).layout

  def format(t: Block): String =
    pretty(toDoc(t), 60).layout

  def format(e: Expr): String =
    pretty(toDoc(e), 60).layout

  val show: PartialFunction[Any, String] = {
    case m: ModuleDecl => format(m).layout
    case d: Definition  => format(List(d))
    case s: Stmt       => format(s)
    case t: ValueType  => format(t)
    case t: BlockType  => format(t)
    case b: Block      => format(b)
    case e: Expr       => format(e)
    case x: Id         => x.show
  }

  val emptyline: Doc = line <> line

  def toDoc(m: ModuleDecl): Doc = {
    "module" <+> m.path <> emptyline <> vsep(m.includes.map { im => "import" <+> im }) <> emptyline <>
      vsep(m.externs.map(toDoc)) <>
      emptyline <>
      vsep(m.declarations.map(toDoc)) <>
      emptyline <>
      toDoc(m.definitions)
  }

  def toDoc(definitions: List[Definition]): Doc =
    vsep(definitions map toDoc, semi)

  def toDoc(e: Extern): Doc = e match {
    case Extern.Def(id, tps, cps, vps, bps, ret, capt, bodies) =>
      "extern" <+> toDoc(capt) <+> "def" <+> toDoc(id) <+> "=" <+> paramsToDoc(tps, vps, bps) <> ":" <+> toDoc(ret) <+> "=" <+> vcat(bodies map {
        case (ff, body) => toDoc(ff) <+> toDoc(body)
      })
    case Extern.Include(ff, contents) => emptyDoc // right now, do not print includes.
  }

  def toDoc(ff: FeatureFlag): Doc = ff match {
    case FeatureFlag.NamedFeatureFlag(name) => name
    case FeatureFlag.Default => "else"
  }

  def toDoc(t: Template[Pure]): Doc =
    /// TODO
    hsep(t.args.map(toDoc), comma)

  def toDoc(b: Block): Doc = b match {
    case BlockVar(id, _, _) => toDoc(id)
    case BlockLit(tps, cps, vps, bps, body) =>
      braces { space <> paramsToDoc(tps, vps, bps) <+> "=>" <+> nest(line <> toDoc(body)) <> line }
    case Member(b, id, _) =>
      toDoc(b) <> "." <> id.name.toString
    case Unbox(e)         => parens("unbox" <+> toDoc(e))
    case New(handler)     => "new" <+> toDoc(handler)
  }

  def toDoc(p: ValueParam): Doc = toDoc(p.id) <> ":" <+> toDoc(p.tpe)
  def toDoc(p: BlockParam): Doc = braces(toDoc(p.id))

  //def toDoc(n: Name): Doc = n.toString

  def toDoc(s: symbols.Symbol): Doc = s.show

  def toDoc(e: Expr): Doc = e match {
    case Literal((), _)            => "()"
    case Literal(s: String, _)     => "\"" + s + "\""
    case Literal(value, _)         => value.toString
    case ValueVar(id, _)           => toDoc(id)

    case PureApp(b, targs, vargs)  => toDoc(b) <> argsToDoc(targs, vargs, Nil)
    case Make(data, tag, vargs)    => "make" <+> toDoc(data) <+> toDoc(tag) <> argsToDoc(Nil, vargs, Nil)
    case DirectApp(b, targs, vargs, bargs) => toDoc(b) <> argsToDoc(targs, vargs, bargs)

    case Select(b, field, tpe) => toDoc(b) <> "." <> toDoc(field)

    case Box(b, capt) => parens("box" <+> toDoc(b))
    case Run(s) => "run" <+> braces(toDoc(s))
  }

  def argsToDoc(targs: List[core.ValueType], vargs: List[core.Pure], bargs: List[core.Block]): Doc =
    val targsDoc = if targs.isEmpty then emptyDoc else brackets(targs.map(toDoc))
    //val cargsDoc = if cargs.isEmpty then emptyDoc else brackets(cargs.map(toDoc))
    val vargsDoc = vargs.map(toDoc)
    val bargsDoc = bargs.map(toDoc)
    targsDoc <> parens(vargsDoc ++ bargsDoc)

  def paramsToDoc(tps: List[symbols.Symbol], vps: List[Param.ValueParam], bps: List[Param.BlockParam]): Doc = {
    val tpsDoc = if (tps.isEmpty) emptyDoc else brackets(tps.map(toDoc))
    tpsDoc <> parens(hsep(vps map toDoc, comma)) <> hcat(bps map toDoc)
  }

  def toDoc(instance: Implementation): Doc = {
    val handlerName = toDoc(instance.interface)
    val clauses = instance.operations.map {
      case Operation(id, tps, cps, vps, bps, resume, body) =>
        val k = resume.map(toDoc).getOrElse(emptyDoc)
        "def" <+> toDoc(id) <> paramsToDoc(tps, vps, bps) <> k <+> "=" <+> nested(toDoc(body))
    }
    handlerName <+> block(vsep(clauses))
  }

  def typeTemplate(kind: Doc, id: symbols.Symbol, tparams: List[symbols.Symbol], decls: List[Doc]): Doc =
    val tps = if tparams.isEmpty then emptyDoc else brackets(tparams.map(toDoc))
    val body = if decls.isEmpty then string("{}") else block(vsep(decls))
    kind <+> toDoc(id) <> tps <+> body

  def toDoc(d: Declaration): Doc = d match {
    case Data(id, tparams, ctors) =>
      typeTemplate("type", id, tparams, ctors.map(toDoc))

    case Interface(id, tparams, properties) =>
      typeTemplate("interface", id, tparams, properties.map(toDoc))
  }

  def toDoc(c: Constructor): Doc = c match {
    case Constructor(id, fields) => toDoc(id) <> parens(fields.map(toDoc))
  }
  def toDoc(f: Field): Doc = f match {
    case Field(name, tpe) => toDoc(name) <> ":" <+> toDoc(tpe)
  }
  def toDoc(f: Property): Doc = f match {
    case Property(name, tpe) => toDoc(name) <> ":" <+> toDoc(tpe)
  }

  def toDoc(d: Definition): Doc = d match {
    case Definition.Def(id, BlockLit(tps, cps, vps, bps, body)) =>
      "def" <+> toDoc(id) <> paramsToDoc(tps, vps, bps) <+> "=" <> nested(toDoc(body))
    case Definition.Def(id, block) =>
      "def" <+> toDoc(id) <+> "=" <+> toDoc(block)
    case Definition.Let(id, binding) =>
      "let" <+> toDoc(id) <+> "=" <+> toDoc(binding)
  }

  def toDoc(s: Stmt): Doc = s match {
    case Scope(definitions, rest) =>
      toDoc(definitions) <> emptyline <> toDoc(rest)

    case Return(e) =>
      toDoc(e)

    case Val(Wildcard(), binding, body) =>
      toDoc(binding) <> ";" <> line <>
        toDoc(body)

    case Val(id, binding, body) =>
      "val" <+> toDoc(id) <+> "=" <+> toDoc(binding) <> ";" <> line <>
        toDoc(body)

    case App(b, targs, vargs, bargs) =>
      toDoc(b) <> argsToDoc(targs, vargs, bargs)

    case If(cond, thn, els) =>
      "if" <+> parens(toDoc(cond)) <+> block(toDoc(thn)) <+> "else" <+> block(toDoc(els))

    case Try(body, hs) =>
      "try" <+> toDoc(body) <+> "with" <+> hsep(hs.map(toDoc), " with")

    case Alloc(id, init, region, body) =>
      "var" <+> toDoc(id) <+> "in" <+> toDoc(region) <+> "=" <+> toDoc(init) <+> ";" <> line <> toDoc(body)

    case Var(id, init, cap, body) =>
      "var" <+> toDoc(id) <+> "=" <+> toDoc(init) <+> ";" <> line <> toDoc(body)

    case Get(id, capt, tpe) =>
      "!" <> toDoc(id)

    case Put(id, capt,  value) =>
      toDoc(id) <+> ":=" <+> toDoc(value)

    case Region(body) =>
      "region" <+> toDoc(body)

    case Match(sc, clauses, default) =>
      val cs = braces(nest(line <> vsep(clauses map { case (p, b) => "case" <+> toDoc(p) <+> toDoc(b) })) <> line)
      val d = default.map { body => space <> "else" <+> braces(nest(line <> toDoc(body))) }.getOrElse { emptyDoc }
      toDoc(sc) <+> "match" <+> cs <> d

    case Hole() =>
      "<>"
  }

  def toDoc(tpe: core.BlockType): Doc = tpe match {
    case core.BlockType.Function(tparams, cparams, vparams, bparams, result) =>
      val tps = if tparams.isEmpty then emptyDoc else brackets(tparams.map(toDoc))
      val vps = parens(vparams.map(toDoc))
      val bps = hcat((cparams zip bparams).map { case (id, tpe) => braces(toDoc(id) <> ":" <+> toDoc(tpe)) })
      val res = toDoc(result)
      tps <> vps <> bps <+> "=>" <+> res
    case core.BlockType.Interface(symbol, Nil) => toDoc(symbol)
    case core.BlockType.Interface(symbol, targs) => toDoc(symbol) <> brackets(targs.map(toDoc))
  }

  def toDoc(tpe: core.ValueType): Doc = tpe match {
    case ValueType.Var(name) => toDoc(name)
    case ValueType.Data(symbol, targs) => toDoc(symbol, targs)
    case ValueType.Boxed(tpe, capt) => toDoc(tpe) <+> "at" <+> toDoc(capt)
  }

  def toDoc(tpeConstructor: symbols.Symbol, targs: List[core.ValueType]): Doc =
    if (targs.isEmpty) then toDoc(tpeConstructor)
    else toDoc(tpeConstructor) <> brackets(targs.map(toDoc))

  def toDoc(capt: core.Captures): Doc = braces(hsep(capt.toList.map(toDoc), comma))

  def nested(content: Doc): Doc = group(nest(line <> content))

  def parens(docs: List[Doc]): Doc = parens(hsep(docs, comma))

  def braces(docs: List[Doc]): Doc = braces(hsep(docs, semi))

  def brackets(docs: List[Doc]): Doc = brackets(hsep(docs, comma))

  def block(content: Doc): Doc = braces(nest(line <> content) <> line)

  def block(docs: List[Doc]): Doc = block(vsep(docs, line))
}
