package effekt
package core

import effekt.core.Type.{PromptSymbol, ResumeSymbol}
import effekt.source.FeatureFlag
import kiama.output.ParenPrettyPrinter

import scala.language.implicitConversions
import effekt.symbols.{Name, Wildcard, builtins}

object PrettyPrinter extends ParenPrettyPrinter {

  import kiama.output.PrettyPrinterTypes.Document

  override val defaultIndent = 2

  def format(t: ModuleDecl): Document =
    pretty(toDoc(t), 4)

  def format(defs: List[Toplevel]): String =
    pretty(toDoc(defs), 60).layout

  def format(s: Stmt): String =
    pretty(toDoc(s), 60).layout

  def format(t: ValueType): String =
    pretty(toDoc(t), 60).layout

  def format(t: BlockType): String =
    pretty(toDoc(t), 60).layout

  def format(t: Block): String =
    pretty(toDoc(t), 60).layout

  def format(p: Expr): String =
    pretty(toDoc(p), 60).layout

  def format(p: Implementation): String =
    pretty(toDoc(p), 60).layout

  val show: PartialFunction[Any, String] = {
    case m: ModuleDecl     => format(m).layout
    case d: Toplevel       => format(List(d))
    case s: Stmt           => format(s)
    case t: ValueType      => format(t)
    case t: BlockType      => format(t)
    case b: Block          => format(b)
    case p: Expr           => format(p)
    case i: Implementation => format(i)
    case x: Id             => x.show
  }

  val emptyline: Doc = line <> line

  def toDoc(m: ModuleDecl): Doc = {
    // The order of toplevel items must match the parser (where the order is currently fixed).
    val includes = vsep(m.includes.map { im => "import" <+> im })
    val decls = vsep(m.declarations.map(toDoc))
    val externs = vsep(m.externs.map(toDoc))
    val defs = toDoc(m.definitions)
    val exports = vsep(m.exports.map { id => "export" <+> toDoc(id) })

    "module" <+> m.path <>
      emptyline <>
      includes <>
      emptyline <>
      decls <>
      emptyline <>
      externs <>
      emptyline <>
      defs <>
      (if m.exports.isEmpty then emptyDoc else emptyline <> exports)
  }

  def toDoc(definitions: List[Toplevel]): Doc =
    vsep(definitions map toDoc)

  def toDoc(e: Extern): Doc = e match {
    case Extern.Def(id, tps, cps, vps, bps, ret, capt, bodies) =>
      "extern" <+> toDoc(capt) <+> "def" <+> toDoc(id) <> paramsToDoc(tps, cps, vps, bps) <> ":" <+> toDoc(ret) <+> "=" <+> (bodies match {
        case ExternBody.StringExternBody(ff, body) => toDoc(ff) <+> toDoc(body)
        // The unsupported case is not currently supported by the core parser
        case ExternBody.Unsupported(err) => ???
      })
    case Extern.Include(ff, contents) => "extern" <+> toDoc(ff) <+> stringLiteral(contents)
  }

  def toDoc(ff: FeatureFlag): Doc = ff match {
    case FeatureFlag.NamedFeatureFlag(name, span) => name
    case FeatureFlag.Default(span) => "default"
  }

  def toDoc(t: Template[Expr]): Doc =
    val Template(strings, args) = t
    val head = stringLiteral(strings.headOption.get)
    val rest: List[Doc] =
      (args zip strings.drop(1)).map { case (e, s) =>
        space <> "++" <+> toDoc(e) <+> "++" <+> stringLiteral(s)
      }
    rest.foldLeft(head)(_ <> _)

  def toDoc(b: Block, preventBraces: Boolean = false): Doc = b match {
    case BlockVar(id, tpe, capt) =>
      toDoc(id) <> ":" <+> toDoc(tpe) <+> "@" <+> toDoc(capt)
    case BlockLit(tps, cps, vps, bps, body) =>
      val doc = space <> paramsToDoc(tps, cps, vps, bps) <+> "=>" <+> nest(line <> toDocStmts(body)) <> line
      if preventBraces then doc else braces { doc }
    case Unbox(e)         => parens("unbox" <+> toDoc(e))
    case New(handler)     => "new" <+> toDoc(handler)
  }

  def toDoc(p: ValueParam): Doc = toDoc(p.id) <> ":" <+> toDoc(p.tpe)
  def toDoc(p: BlockParam): Doc = braces(toDoc(p.id) <> ":" <+> toDoc(p.tpe))
  def toDoc(cparam: Id, bparam: BlockParam): Doc = braces(toDoc(bparam.id) <+> "@" <+> toDoc(cparam) <> ":" <+> toDoc(bparam.tpe))

  //def toDoc(n: Name): Doc = n.toString

  def toDoc(s: symbols.Symbol): Doc = {
    builtins.coreBuiltinSymbolToString(s).getOrElse(s.name.name + "$" + s.id)
  }

  def toDoc(e: Expr): Doc = e match {
    case Literal((), _)            => "()"
    case Literal(s: String, _)     => stringLiteral(s)
    case Literal(value, _)         => value.toString
    case ValueVar(id, tpe)         => toDoc(id) <> ":" <+> toDoc(tpe)

    case PureApp(b, targs, vargs)  => parens(toDoc(b)) <> argsToDoc(targs, vargs, Nil)
    case Make(data, tag, targs, vargs)    => "make" <+> toDoc(data) <+> toDoc(tag) <> argsToDoc(targs, vargs, Nil)

    case Box(b, capt) => "box" <+> toDoc(capt) <+> toDoc(b)
  }

  def argsToDoc(targs: List[core.ValueType], vargs: List[core.Expr], bargs: List[core.Block]): Doc =
    val targsDoc = if targs.isEmpty then emptyDoc else brackets(targs.map(toDoc))
    val vargsDoc = parens(vargs.map(toDoc))

    // Wrap in braces individually, then concat with a space between. Force BlockLits to not add a layer of braces on top.
    val bargsDoc =
      if bargs.isEmpty then emptyDoc
      else hsep { bargs.map { b => braces(toDoc(b, preventBraces = true)) } }
    targsDoc <> vargsDoc <> bargsDoc

  private def typeParamsDoc(tps: List[symbols.Symbol]): Doc =
    if tps.isEmpty then emptyDoc else brackets(tps.map(tp => string("'") <> toDoc(tp)))

  def paramsToDoc(tps: List[symbols.Symbol], cps: List[Id], vps: List[ValueParam], bps: List[BlockParam]): Doc = {
    val tpsDoc = typeParamsDoc(tps)
    val vpsDoc = parens(vps.map(toDoc))
    val bpsDoc = if bps.isEmpty then emptyDoc else hsep(cps.zip(bps).map(toDoc(_, _)))
    tpsDoc <> vpsDoc <> bpsDoc
  }

  def toDoc(instance: Implementation): Doc = {
    val handlerName = toDoc(instance.interface)
    val clauses = instance.operations.map {
      case Operation(id, tps, cps, vps, bps, body) =>
        "def" <+> toDoc(id) <> paramsToDoc(tps, cps, vps, bps) <+> "=" <+> block(toDocStmts(body))
    }
    handlerName <+> block(vsep(clauses))
  }

  def typeTemplate(kind: Doc, id: symbols.Symbol, tparams: List[symbols.Symbol], decls: List[Doc]): Doc =
    val tps = typeParamsDoc(tparams)
    val body = if decls.isEmpty then string("{}") else block(vsep(decls))
    kind <+> toDoc(id) <> tps <+> body

  def toDoc(d: Declaration): Doc = d match {
    case Data(id, tparams, ctors) =>
      typeTemplate("type", id, tparams, ctors.map(toDoc))

    case Interface(id, tparams, properties) =>
      typeTemplate("interface", id, tparams, properties.map(toDoc))
  }

  def toDoc(c: Constructor): Doc = c match {
    case Constructor(id, tparams, fields) => toDoc(id) <> typeParamsDoc(tparams) <> parens(fields.map(toDoc))
  }
  def toDoc(f: Field): Doc = f match {
    case Field(name, tpe) => toDoc(name) <> ":" <+> toDoc(tpe)
  }
  def toDoc(f: Property): Doc = f match {
    case Property(name, tpe) => toDoc(name) <> ":" <+> toDoc(tpe)
  }

  def toDoc(d: Toplevel): Doc = d match {
    case Toplevel.Def(id, BlockLit(tps, cps, vps, bps, body)) =>
      "def" <+> toDoc(id) <> paramsToDoc(tps, cps, vps, bps) <+> "=" <+> block(toDocStmts(body))
    case Toplevel.Def(id, blockv) =>
      "def" <+> toDoc(id) <+> "=" <+> toDoc(blockv)
    case Toplevel.Val(id, binding) =>
      "val" <+> toDoc(id) <+> "=" <+> toDoc(binding)
  }

  def toDoc(s: Stmt): Doc = s match {
    // requires a block to be readable:
    case _ : (Stmt.Def | Stmt.Let | Stmt.Val | Stmt.Alloc | Stmt.Var | Stmt.Get | Stmt.Put) => block(toDocStmts(s))
    case other => toDocStmts(s)
  }

  private def toDocSingleCapture(capt: core.Captures): Doc =
    capt.toList match {
      case x :: Nil => toDoc(x)
      case _        => toDoc(capt)
    }

  def toDocStmts(s: Stmt): Doc = s match {
    case Def(id, BlockLit(tps, cps, vps, bps, body), rest) =>
      // RHS must be a single `stmt`, so we have to wrap it in a block.
      "def" <+> toDoc(id) <> paramsToDoc(tps, cps, vps, bps) <+> "=" <+> block(toDocStmts(body)) <> line <>
        toDocStmts(rest)

    case Def(id, block, rest) =>
      "def" <+> toDoc(id) <+> "=" <+> toDoc(block) <> line <>
        toDocStmts(rest)

    case Let(id, binding, rest) =>
      "let" <+> toDoc(id) <+> "=" <+> toDoc(binding) <> line <>
        toDocStmts(rest)

    case ImpureApp(id, callee, targs, vargs, bargs, rest) =>
      "let" <+> "!" <+> toDoc(id) <+> "=" <+> toDoc(callee) <> argsToDoc(targs, vargs, bargs) <> line <>
        toDocStmts(rest)

    case Return(e) =>
      "return" <+> toDoc(e)

    case Val(id, binding, body) =>
      // RHS must be a single `stmt`, so we have to wrap it in a block.
      "val" <+> toDoc(id) <+> "=" <+> block(toDocStmts(binding)) <> ";" <> line <>
        toDocStmts(body)

    case App(b, targs, vargs, bargs) =>
      toDoc(b) <> argsToDoc(targs, vargs, bargs)

    case Invoke(b, method, methodTpe, targs, vargs, bargs) =>
      toDoc(b) <> "." <> toDoc(method) <> ":" <+> toDoc(methodTpe) <> argsToDoc(targs, vargs, bargs)

    case If(cond, thn, els) =>
      "if" <+> parens(toDoc(cond)) <+> block(toDocStmts(thn)) <+> "else" <+> block(toDocStmts(els))

    case Reset(body) =>
      "reset" <+> toDoc(body)

    case Shift(prompt, k, body) =>
      "shift" <> parens(toDoc(prompt)) <+> braces { toDoc(k) <+> "=>" <+> nest(line <> toDocStmts(body)) <> line }

    case Resume(k, body) =>
      "resume" <> parens(toDoc(k)) <+> block(toDocStmts(body))

    case Alloc(id, init, region, body) =>
      "var" <+> toDoc(id) <+> "in" <+> toDoc(region) <+> "=" <+> toDoc(init) <> ";" <> line <>
        toDocStmts(body)

    case Var(ref, init, cap, body) =>
      "var" <+> toDoc(ref) <+> "@" <+> toDoc(cap) <+> "=" <+> toDoc(init) <> ";" <> line <>
        toDocStmts(body)

    case Get(id, tpe, ref, capt, body) =>
      "get" <+> toDoc(id) <+> ":" <+> toDoc(tpe) <+> "=" <+> "!" <+> toDoc(ref) <+> "@" <+> toDocSingleCapture(capt) <> ";" <> line <>
        toDocStmts(body)

    case Put(ref, capt, value, body) =>
      "put" <+> toDoc(ref) <+> "@" <+> toDocSingleCapture(capt) <+> "=" <+> toDoc(value) <> ";" <> line <>
        toDocStmts(body)

    case Region(body) =>
      "region" <+> toDoc(body)

    case Match(sc, tpe, clauses, default) =>
      val cs = if clauses.isEmpty then string("{}") else braces(nest(line <> vsep(clauses map { case (p, b) => toDoc(p) <+> ":" <+> toDoc(b) })) <> line)
      val d = default.map { body => space <> "else" <+> braces(nest(line <> toDocStmts(body))) }.getOrElse { emptyDoc }
      toDoc(sc) <+> "match" <> brackets(toDoc(tpe)) <+> cs <> d

    case Hole(tpe, span) =>
      val from = span.from
      val to = span.to
      val name = span.source.name
      val scheme = span.source match {
        case _: kiama.util.FileSource   => "file"
        case _: kiama.util.StringSource => "string"
        case _                          => "source"
      }
      "<>" <> ":" <+> toDoc(tpe) <+> s"@ \"$scheme://$name\":$from:$to"
  }

  def toDoc(tpe: core.BlockType): Doc = tpe match {
    case core.BlockType.Function(tparams, cparams, vparams, bparams, result) =>
      val tps = typeParamsDoc(tparams)
      val vps = parens(vparams.map(toDoc))
      val bps = hsep((cparams zip bparams).map { case (id, tpe) => braces(toDoc(id) <> ":" <+> toDoc(tpe)) })
      // After `=>` the grammar expects a primValueType. If the result is a boxed value type
      // (i.e., contains `at { ... }`), we must parenthesize it so it parses via `( valueType )`.
      val res =
        result match {
          case core.ValueType.Boxed(_, _) => parens(toDoc(result))
          case _                          => toDoc(result)
        }
      tps <> vps <> bps <+> "=>" <+> res
    case core.BlockType.Interface(symbol, Nil) => toDoc(symbol)
    case core.BlockType.Interface(symbol, targs) => toDoc(symbol) <> brackets(targs.map(toDoc))
  }

  def toDoc(tpe: core.ValueType): Doc = tpe match {
    case ValueType.Var(name) => "'" <> toDoc(name)
    case ValueType.Data(symbol, targs) => toDoc(symbol, targs)
    case ValueType.Boxed(tpe, capt) => toDoc(tpe) <+> "at" <+> toDoc(capt)
  }

  def toDoc(tpeConstructor: symbols.Symbol, targs: List[core.ValueType]): Doc =
    if (targs.isEmpty) then toDoc(tpeConstructor)
    else toDoc(tpeConstructor) <> brackets(targs.map(toDoc))

  def toDoc(capt: core.Captures): Doc = braces(hsep(capt.toList.sortBy(_.name.name).map(toDoc), comma))

  def nested(content: Doc): Doc = group(nest(line <> content))

  def parens(docs: List[Doc]): Doc = parens(hsep(docs, comma))

  def braces(docs: List[Doc]): Doc = braces(hsep(docs, semi))

  def brackets(docs: List[Doc]): Doc = brackets(hsep(docs, comma))

  def block(content: Doc): Doc = braces(nest(line <> content) <> line)

  def block(docs: List[Doc]): Doc = block(vsep(docs, line))

  private def escapeString(s: String): String = {
    // TODO: Unicode escapes?
    s.flatMap {
      case '\\' => "\\\\"
      case '"'  => "\\\""
      case '\r' => "\\r"
      case '\t' => "\\t"
      case c    => c.toString
    }
  }

  def stringLiteral(s: String): Doc =
    if s.contains("\n") then multilineStringLiteral(s)
    else "\"" <> escapeString(s) <> "\""

  def multilineStringLiteral(s: String): Doc = {
    val multi = "\"\"\""
    multi <> s <> multi
  }
}
