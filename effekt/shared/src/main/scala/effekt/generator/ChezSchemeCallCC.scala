package effekt.generator

import effekt.context.Context
import effekt.core._
import effekt.symbols.Module
import effekt.symbols.{ Name, Symbol, Wildcard }

import kiama.output.ParenPrettyPrinter
import kiama.output.PrettyPrinterTypes.Document
import kiama.util.Source
import effekt.context.assertions._

import scala.language.implicitConversions

import effekt.util.paths._

/**
 * It would be nice if Core could have an Effect Declaration or
 * translate effect declarations to Records...
 */
class ChezSchemeCallCC extends Generator {

  /**
   * This is used for both: writing the files to and generating the `require` statements.
   */
  def path(m: Module)(implicit C: Context): String =
    (C.config.outputPath() / m.path.replace('/', '_')).unixPath + ".ss"

  /**
   * This is only called on the main entry point, we have to manually traverse the dependencies
   * and write them.
   */
  def run(src: Source)(implicit C: Context): Option[Document] = for {
    mod <- C.frontend(src)
    _ = C.checkMain(mod)
    deps = mod.dependencies.flatMap(dep => compile(dep))
    core <- C.backend(src)
    result = ChezSchemeCallCCPrinter.compilationUnit(mod, core, deps)
    _ = C.saveOutput(result.layout, path(mod))
  } yield result

  /**
   * Compiles only the given module, does not compile dependencies
   */
  def compile(mod: Module)(implicit C: Context): Option[Document] = for {
    core <- C.backend(mod.source)
    doc = ChezSchemeCallCCPrinter.format(core)
  } yield doc
}

object ChezSchemeCallCCPrinter extends ChezSchemeBase {

  def compilationUnit(mod: Module, core: ModuleDecl, dependencies: List[Document])(implicit C: Context): Document =
    pretty {

      val main = mod.terms("main").toList.head

      prelude <>
        "(let () " <+> emptyline <>
        vsep(dependencies.map { m => string(m.layout) }) <>
        module(core) <> emptyline <>
        defineValue("main", nameDef(main)) <> emptyline <>
        "(run " <> nameRef(main) <> "))"
    }

  override def toDoc(b: Block)(implicit C: Context): Doc = link(b, b match {
    case BlockVar(v) =>
      nameRef(v)
    case BlockLit(ps, body) =>
      schemeLambda(ps map toDoc, toDoc(body, false))
    case Member(b, id) =>
      schemeCall(nameRef(id), toDoc(b))
    case Extern(ps, body) =>
      schemeLambda(ps map toDoc, body)

    case ScopeApp(b, sc) => ???
    case ScopeAbs(id, b) => ???
    case Lifted(ev, b)   => ???
    case Unbox(e)        => toDoc(e)
  })

  override def toDoc(s: Stmt, toplevel: Boolean)(implicit C: Context): Doc = s match {
    case State(eff, tpe, get, put, init, block) =>
      defineValue(nameDef(get), "getter") <> line <>
        defineValue(nameDef(put), "setter") <> line <>
        schemeCall("state", toDoc(init, false), toDoc(block))

    case other => super.toDoc(s, toplevel)
  }

  override def requiresBlock(s: Stmt): Boolean = s match {
    // this is only true for the direct-style PP
    case _: Val => true
    case _      => super.requiresBlock(s)
  }
}

trait ChezSchemeBase extends ParenPrettyPrinter {

  import kiama.output.PrettyPrinterTypes.Document

  val prelude = "#!/usr/local/bin/scheme --script\n\n(import (chezscheme))\n\n"

  def moduleFile(path: String): String = path.replace('/', '_') + ".ss"

  def format(t: ModuleDecl)(implicit C: Context): Document =
    pretty(module(t))

  val emptyline: Doc = line <> line

  def module(m: ModuleDecl)(implicit C: Context): Doc = toDoc(m)

  def toDoc(m: ModuleDecl)(implicit C: Context): Doc =
    toDoc(m.defs, true)

  def toDoc(b: Block)(implicit C: Context): Doc

  def toDoc(p: Param)(implicit C: Context): Doc = link(p, nameDef(p.id))

  def toDoc(n: Name)(implicit C: Context): Doc = link(n, n.toString)

  // we prefix op$ to effect operations to avoid clashes with reserved names like `get` and `set`
  def nameDef(id: Symbol)(implicit C: Context): Doc =
    id.name.toString + "_" + id.id

  def nameRef(id: Symbol)(implicit C: Context): Doc =
    id.name.toString + "_" + id.id

  def toDoc(e: Expr)(implicit C: Context): Doc = link(e, e match {
    case UnitLit()     => "#f"
    case StringLit(s)  => jsString(s)
    case BooleanLit(b) => if (b) "#t" else "#f"
    case l: Literal[t] => l.value.toString
    case ValueVar(id)  => nameRef(id)

    case PureApp(b, targs, args) => schemeCall(toDoc(b), args map {
      case e: Expr  => toDoc(e)
      case b: Block => toDoc(b)
    })

    case Select(b, field) =>
      schemeCall(nameRef(field), toDoc(b))

    case Closure(b) => toDoc(b)
  })

  def argToDoc(e: Argument)(implicit C: Context): Doc = e match {
    case e: Expr  => toDoc(e)
    case b: Block => toDoc(b)
  }

  def toDoc(s: Stmt, toplevel: Boolean)(implicit C: Context): Doc = s match {

    case If(cond, thn, els) =>
      parens("if" <+> toDoc(cond) <+> toDocInBlock(thn) <+> toDocInBlock(els))

    case While(cond, body) =>
      parens("while" <+> toDocInBlock(cond) <+> toDocInBlock(body))

    // definitions can *only* occur at the top of a (begin ...)
    case Def(id, tpe, BlockLit(ps, body), rest) =>
      defineFunction(nameDef(id), ps map toDoc, toDoc(body, false)) <> emptyline <> toDoc(rest, toplevel)

    // we can't use the unique id here, since we do not know it in the extern string.
    case Def(id, tpe, Extern(ps, body), rest) =>
      defineFunction(nameDef(id), ps.map { p => p.id.name.toString }, body) <> emptyline <> toDoc(rest, toplevel)

    case Data(did, ctors, rest) =>
      val cs = ctors.map { ctor => generateConstructor(ctor.asConstructor) }
      vsep(cs) <> emptyline <> toDoc(rest, toplevel)

    case Record(did, fields, rest) =>
      generateConstructor(did, fields) <> emptyline <> toDoc(rest, toplevel)

    case Include(contents, rest) =>
      line <> vsep(contents.split('\n').toList.map(c => text(c))) <> emptyline <> toDoc(rest, toplevel)

    case App(b, targs, args) => schemeCall(toDoc(b), args map {
      case e: Expr  => toDoc(e)
      case b: Block => toDoc(b)
    })

    case Val(Wildcard(_), tpe, binding, body) if toplevel =>
      "(run (thunk " <> toDoc(binding, false) <> "))" <> line <> toDoc(body, true)

    case Val(id, tpe, binding, body) if toplevel =>
      defineValue(nameDef(id), "(run (thunk " <> toDocInBlock(binding) <> "))") <> line <> toDoc(body, toplevel)

    case Val(Wildcard(_), tpe, binding, body) =>
      toDoc(binding, false) <> line <> toDocInBlock(body)

    case Val(id, tpe, binding, body) =>
      parens("let" <+> parens(brackets(nameDef(id) <+> toDocInBlock(binding))) <+> group(nest(line <> toDoc(body, false))))

    case Ret(e) => toDoc(e)

    case Handle(body, handler: List[Handler]) =>
      val handlers: List[Doc] = handler.map { h =>

        brackets("make-" <> nameRef(h.id) <+> vsep(h.clauses.map {
          case (op, impl) =>
            // the LAST argument is the continuation...
            val params = impl.params.init
            val kParam = impl.params.last

            parens(nameDef(op) <+>
              parens(hsep(params.map { p => nameRef(p.id) }, space)) <+>
              nameRef(kParam.id) <+>
              toDocInBlock(impl.body))
        }, line))
      }
      parens("handle" <+> parens(vsep(handlers)) <+> toDoc(body))

    case Match(sc, cls) =>
      val clauses: List[Doc] = cls map {

        // curry the block
        case (pattern, b: BlockLit) =>
          brackets(toDoc(pattern) <+> b.params.foldRight(schemeLambda(Nil, toDoc(b.body, false))) {
            case (p, body) => schemeLambda(List(nameDef(p.id)), body)
          })

        case (pattern, b) => sys error "Right hand side of a matcher needs to be a block definition"
      }
      schemeCall("pattern-match", toDoc(sc) :: parens(vsep(clauses, line)) :: Nil)

    // only export main for now
    case Exports(path, exports) => ""

    case other                  => sys error s"Can't print: ${other}"
  }

  def toDoc(p: Pattern)(implicit C: Context): Doc = p match {
    case IgnorePattern()    => "ignore"
    case AnyPattern()       => "any"
    case LiteralPattern(l)  => schemeCall("literal", toDoc(l))
    case TagPattern(id, ps) => schemeCall("match-" <> nameDef(id), ps map { p => toDoc(p) })
  }

  def generateConstructor(ctor: effekt.symbols.Record)(implicit C: Context): Doc =
    generateConstructor(ctor, ctor.fields)

  // https://www.scheme.com/csug8/objects.html
  // https://scheme.com/tspl4/records.html
  def generateConstructor(did: Symbol, fields: List[Symbol])(implicit C: Context): Doc = {
    val pred = nameDef(did) <> "?"
    val matcher = "match-" <> nameDef(did)

    // rethrowing an effect causes some unexpected shadowing since the constructor and effect names are called the same.
    val constructor = if (did.isInstanceOf[effekt.symbols.Effect]) "make-" <> nameDef(did) else nameDef(did)

    val definition =
      parens("define-record-type" <+> parens(did.name.toString <+> constructor <+> pred) <>
        nest(line <> parens("fields" <+> nest(line <> vsep(fields.map { f => parens("immutable" <+> nameDef(f) <+> nameDef(f)) }))) <> line <>
          parens("nongenerative" <+> nameDef(did))))

    var fresh = 0;

    val matcherDef =
      parens("define-matcher" <+> matcher <+> pred <>
        nest(line <> parens(vsep(fields.map { f =>
          fresh += 1
          brackets(s"p${fresh}" <+> nameDef(f))
        }))))

    s";;; Record definition for ${did.name}" <> line <> definition <> line <> matcherDef
  }

  def defineValue(name: Doc, binding: Doc): Doc =
    parens("define" <+> name <+> binding)

  def defineFunction(name: Doc, params: List[Doc], body: Doc): Doc =
    parens("define" <+> name <+> schemeLambda(params, body))

  def schemeLambda(params: List[Doc], body: Doc): Doc =
    parens("lambda" <+> parens(hsep(params, space)) <> group(nest(line <> body)))

  def jsString(contents: Doc): Doc =
    "\"" <> contents <> "\""

  def schemeCall(fun: Doc, args: Doc*): Doc = schemeCall(fun, args.toList)
  def schemeCall(fun: Doc, args: List[Doc]): Doc = parens(hsep(fun :: args, space))

  // printing blocks with let prevents locally defined mutually recursive functions
  def toDocInBlock(s: Stmt)(implicit C: Context): Doc =
    if (requiresBlock(s))
      parens("let ()" <+> nest(line <> toDoc(s, false)))
    else
      toDoc(s, false)

  def requiresBlock(s: Stmt): Boolean = s match {
    case _: Data   => true
    case _: Record => true
    case _: Def    => true
    // State requires a block since it inserts a definition
    case _: State  => true
    case _         => false
  }
}
