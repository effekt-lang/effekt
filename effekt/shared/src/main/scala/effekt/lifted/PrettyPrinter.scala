package effekt
package lifted

import effekt.source.FeatureFlag
import kiama.output.ParenPrettyPrinter

import scala.language.implicitConversions
import effekt.symbols.{Name, Wildcard, builtins}

object PrettyPrinter extends ParenPrettyPrinter {

  import kiama.output.PrettyPrinterTypes.Document

  override val defaultIndent = 2

  def format(t: ModuleDecl): Document =
    pretty(toDoc(t), 4)

  def format(s: Stmt): String =
    pretty(toDoc(s), 60).layout

  def format(b: Block): String =
    pretty(toDoc(b), 60).layout

  def format(e: Expr): String =
    pretty(toDoc(e), 60).layout

  def format(defs: List[Definition]): String =
    pretty(toDoc(defs), 60).layout

  val show: PartialFunction[Any, String] = {
    case m: ModuleDecl => format(m).layout
    case d: Definition  => format(List(d))
    case s: Stmt       => format(s)
    case b: Block      => format(b)
    case e: Expr      => format(e)
    case x: Id         => x.show
  }

  val emptyline: Doc = line <> line

  def toDoc(m: ModuleDecl): Doc = {
    "module" <+> m.path <> emptyline <> vsep(m.includes.map { im => "import" <+> im }, line) <>
      emptyline <> vcat(m.externs.map(toDoc)) <>
      emptyline <> vcat(m.decls.map(toDoc)) <>
      emptyline <> toDoc(m.definitions)
  }

  def signature(tparams: List[Id], params: List[Param], ret: ValueType): Doc =
    val tps = if tparams.isEmpty then emptyDoc else brackets(tparams.map(toDoc))
    val ps = parens(params.map(toDoc))
    tps <> ps <> ":" <+> toDoc(ret)

  def toDoc(p: Param): Doc = p match {
    case Param.ValueParam(id, tpe) => id.name.toString <> ":" <+> toDoc(tpe)
    case Param.BlockParam(id, tpe) => id.name.toString <> ":" <+> toDoc(tpe)
    case Param.EvidenceParam(id) => id.name.toString <> ": EV"
  }

  def toDoc(e: Extern): Doc = e match {
    case Extern.Def(id, tparams, params, ret, bodies) =>
      "extern def" <+> toDoc(id.name) <> signature(tparams, params, ret) <+> "=" <+> "\"" <> vsep(bodies map {
        (ff, body) => toDoc(ff) <> toDoc(body)
      }) <> "\""
    case Extern.Include(ff, contents) => emptyDoc // right now, do not print includes.
  }

  def toDoc(ff: FeatureFlag): Doc = ff match {
    case FeatureFlag.NamedFeatureFlag(name) => name
    case FeatureFlag.Default => "else"
  }

  // TODO implement
  def toDoc(t: Template[Expr]): Doc =
    hsep(t.args.map(toDoc), comma)

  def toDoc(b: Block): Doc = b match {
    case BlockVar(v, _) => v.name.toString
    case BlockLit(tps, ps, body) =>
      braces { space <> parens(hsep(ps.map(toDoc), comma)) <+> "=>" <+> nest(line <> toDoc(body)) <> line }
    case Member(b, id, tpe) =>
      toDoc(b) <> "." <> id.name.toString
    case Unbox(e)         => parens("unbox" <+> toDoc(e))
    case New(handler)     => "new" <+> toDoc(handler)
  }

  def toDoc(n: Name): Doc = n.toString

  def toDoc(s: symbols.Symbol): Doc = toDoc(s.name)

  def toDoc(e: Expr): Doc = e match {
    case Literal((), _)          => "()"
    case Literal(s: String, _)   => "\"" + s + "\""
    case l: Literal              => l.value.toString
    case ValueVar(id, _)         => id.name.toString

    case Make(data, tag, args) =>
      "make" <+> toDoc(data) <+> toDoc(tag) <> parens(hsep(args map argToDoc, comma))

    case PureApp(b, targs, args) =>
      val ts = if targs.isEmpty then emptyDoc else brackets(targs.map(toDoc))
      toDoc(b) <> ts <> parens(hsep(args map argToDoc, comma))

    case Select(b, field, tpe) =>
      toDoc(b) <> "." <> toDoc(field.name)

    case Box(b) => parens("box" <+> toDoc(b))
    case Run(s)  => "run" <+> block(toDoc(s))
  }

  def argToDoc(e: Argument): Doc = e match {
    case e: Expr     => toDoc(e)
    case b: Block    => toDoc(b)
    case e: Evidence => toDoc(e)
  }

  def toDoc(e: Evidence): Doc = e match {
    case Evidence(Nil)  => "<>"
    case Evidence(list) => angles(hsep(list.map { ev => toDoc(ev) }, ","))
  }

  def toDoc(l: Lift): Doc = l match {
    case Lift.Var(ev) => toDoc(ev.name)
    case Lift.Try() => "Try"
    case Lift.Reg() => "Reg"
  }

  def toDoc(impl: Implementation): Doc = {
    val handlerName = toDoc(impl.interface.name.name)
    val clauses = impl.operations.map {
      case Operation(id, BlockLit(tparams, params, body)) =>
        "def" <+> toDoc(id.name) <> parens(params.map(toDoc)) <+> "=" <+> nested(toDoc(body))
    }
    handlerName <+> block(vsep(clauses))
  }

  def toDoc(d: Declaration): Doc = d match {
    case Declaration.Data(did, tparams, ctors) =>
      val tps = if tparams.isEmpty then emptyDoc else brackets(tparams.map(toDoc))
      "type" <+> toDoc(did.name) <> tps <+> braces(ctors.map(toDoc))

    case Declaration.Interface(id, tparams, operations) =>
      val tps = if tparams.isEmpty then emptyDoc else brackets(tparams.map(toDoc))
      "interface" <+> toDoc(id.name) <> tps <+> braces(operations.map(toDoc))
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
    case Definition.Def(id, BlockLit(tparams, params, body)) =>
      "def" <+> toDoc(id.name) <> signature(tparams, params, body.tpe) <+> "=" <> nested(toDoc(body))
    case Definition.Def(id, block) =>
      "def" <+> toDoc(id.name) <+> "=" <+> toDoc(block)
    case Definition.Let(id, binding) =>
      "let" <+> toDoc(id.name) <+> "=" <+> toDoc(binding)
  }

  def toDoc(definitions: List[Definition]): Doc =
    vsep(definitions map toDoc, semi)

  def toDoc(s: Stmt): Doc = s match {
    case Scope(definitions, rest) =>
      toDoc(definitions) <> emptyline <> toDoc(rest)

    case Val(Wildcard(), binding, body) =>
      toDoc(binding) <> ";" <> line <>
        toDoc(body)

    case Val(id, binding, body) =>
      "val" <+> toDoc(id.name) <+> "=" <+> toDoc(binding) <> ";" <> line <>
        toDoc(body)

    case App(b, targs, args) =>
      val ts = if targs.isEmpty then emptyDoc else brackets(targs.map(toDoc))
      toDoc(b) <> ts <> parens(hsep(args map argToDoc, comma))

    case If(cond, thn, els) =>
      "if" <+> parens(toDoc(cond)) <+> block(toDoc(thn)) <+> "else" <+> block(toDoc(els))

    case Return(e) =>
      toDoc(e)

    case Try(body, hs) =>
      "try" <+> toDoc(body) <+> "with" <+> hsep(hs.map(toDoc), " with")

    case Reset(body) =>
      "reset" <+> block(toDoc(body))

    case Shift(ev, body) =>
      "shift" <> parens(toDoc(ev)) <+> toDoc(body)

    case Alloc(id, init, region, ev, body) =>
      "var" <+> toDoc(id.name) <+> "in" <+> toDoc(region.name) <+> "=" <+> toDoc(init) <+> ";" <> line <> toDoc(body)

    case Var(init, body) =>
      "state" <+> parens(toDoc(init)) <+> toDoc(body)

    case Get(id, ev, tpe) =>
      "get" <+>  toDoc(id.name) <> parens(toDoc(ev))

    case Put(id, ev, value) =>
      "put" <+> toDoc(id.name) <> parens(List(toDoc(ev), toDoc(value)))

    case Region(body) =>
      "region" <+> toDoc(body)

    case Match(sc, clauses, default) =>
      val cs = braces(nest(line <> vsep(clauses map { case (p, b) => "case" <+> toDoc(p.name) <+> toDoc(b) })) <> line)
      val d = default.map { body => space <> "else" <+> braces(nest(line <> toDoc(body))) }.getOrElse { emptyDoc }
      toDoc(sc) <+> "match" <+> cs <> d

    case Hole() =>
      "<>"
  }


  def toDoc(tpe: lifted.BlockType): Doc = tpe match {
    case lifted.BlockType.Function(tparams, eparams, vparams, bparams, result) =>
      val tps = if tparams.isEmpty then emptyDoc else brackets(tparams.map(toDoc))
      val eps = eparams.map { _ => string("EV") }
      val vps = vparams.map(toDoc)
      val bps = bparams.map(toDoc)
      val res = toDoc(result)
      tps <> parens(eps ++ vps ++ bps) <+> "=>" <+> res
    case lifted.BlockType.Interface(symbol, Nil) => toDoc(symbol)
    case lifted.BlockType.Interface(symbol, targs) => toDoc(symbol) <> brackets(targs.map(toDoc))
  }

  def toDoc(tpe: lifted.ValueType): Doc = tpe match {
    case lifted.ValueType.Var(name) => toDoc(name)
    case lifted.ValueType.Data(symbol, targs) => toDoc(symbol, targs)
    case lifted.ValueType.Boxed(tpe) => "box" <+> toDoc(tpe)
  }

  def toDoc(tpeConstructor: symbols.Symbol, targs: List[lifted.ValueType]): Doc =
    if (targs.isEmpty) then toDoc(tpeConstructor)
    else toDoc(tpeConstructor) <> brackets(targs.map(toDoc))

  def nested(content: Doc): Doc = group(nest(line <> content))

  def parens(docs: List[Doc]): Doc = parens(hsep(docs, comma))

  def braces(docs: List[Doc]): Doc = braces(hsep(docs, semi))

  def brackets(docs: List[Doc]): Doc = brackets(hsep(docs, comma))

  def block(content: Doc): Doc = braces(nest(line <> content) <> line)

  def block(docs: List[Doc]): Doc = block(vsep(docs, line))

}
