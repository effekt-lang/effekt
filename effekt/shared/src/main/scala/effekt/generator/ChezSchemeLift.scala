package effekt
package generator

import effekt.context.Context
import effekt.lifted.*
import effekt.symbols.{ Module, Wildcard, Name }
import effekt.symbols.builtins.TState

import kiama.output.PrettyPrinterTypes.Document
import kiama.util.Source

import scala.language.implicitConversions
import effekt.context.assertions._

import effekt.util.paths.*

object ChezSchemeLift extends Backend {

  /**
   * Returns [[Compiled]], containing the files that should be written to.
   */
  def compileWhole(main: CoreTransformed, dependencies: List[CoreTransformed])(implicit C: Context) = {
    C.checkMain(main.mod)
    val mainFile = path(main.mod)
    val deps = dependencies.flatMap { dep => compile(dep) }
    LiftInference(main).map { lifted =>
      val result = ChezSchemeLiftPrinter.compilationUnit(main.mod, lifted.core, deps)
      Compiled(mainFile, Map(mainFile -> result))
    }
  }

  /**
   * Entrypoint used by the LSP server to show the compiled output
   */
  def compileSeparate(input: CoreTransformed)(implicit C: Context) =
    C.using(module = input.mod) { compile(input) }

  /**
   * Compiles only the given module, does not compile dependencies
   */
  private def compile(in: CoreTransformed)(implicit C: Context): Option[Document] =
    LiftInference(in).map { lifted => ChezSchemeLiftPrinter.format(lifted.core) }

  /**
   * This is used for both: writing the files to and generating the `require` statements.
   */
  def path(m: Module)(implicit C: Context): String =
    (C.config.outputPath() / m.path.replace('/', '_')).unixPath + ".ss"
}

object ChezSchemeLiftPrinter extends ChezSchemeLiftedBase {

  def compilationUnit(mod: Module, core: ModuleDecl, dependencies: List[Document])(implicit C: Context): Document =
    pretty {

      val main = mod.terms("main").toList.head

      prelude <>
        "(let () " <+> emptyline <>
        vsep(dependencies.map { m => string(m.layout) }) <>
        module(core) <> emptyline <>
        "(run ((" <> nameRef(main) <> " here))))"
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

    case ScopeApp(b, sc) => schemeCall(toDoc(b), List(toDoc(sc)))
    case ScopeAbs(id, b) => schemeLambda(List(nameDef(id)), toDoc(b))
    case Lifted(ev, b)   => schemeCall("lift-block", List(toDoc(b), toDoc(ev)))
    case Unbox(e)        => toDoc(e)
  })

  override def toDoc(s: Stmt, toplevel: Boolean)(implicit C: Context): Doc = s match {
    case Val(Wildcard(_), tpe, binding, body) if toplevel =>
      "(run " <> toDoc(binding, false) <> ")" <> emptyline <> toDoc(body, toplevel)

    case Val(id, tpe, binding, body) if toplevel =>
      defineValue(nameDef(id), "(run " <> toDoc(binding, false) <> ")") <> emptyline <> toDoc(body, toplevel)

    case Val(Wildcard(_), tpe, binding, body) =>
      schemeCall("then", toDoc(binding, false), "_", toDoc(body, false))

    case Let(id, tpe, binding, body) if toplevel =>
      defineValue(nameDef(id), toDoc(binding)) <> line <> toDoc(body, toplevel)

    case Let(id, tpe, binding, body) =>
      parens("let" <+> parens(brackets(nameDef(id) <+> toDoc(binding))) <> group(nest(line <> toDoc(body, toplevel))))

    // do not return on the toplevel
    case Ret(e) if toplevel => ""

    case Ret(e)             => schemeCall("pure", List(toDoc(e)))

    case Val(id, tpe, binding, body) =>
      schemeCall("then", toDoc(binding, false), nameDef(id), toDoc(body, false))

    case Def(id, tpe, ScopeAbs(sc, BlockLit(ps, body)), rest) =>
      defineFunction(nameDef(id), List(nameDef(sc)),
        schemeLambda(ps map toDoc, toDoc(body, false))) <> emptyline <> toDoc(rest, toplevel)

    case State(id, init, reg, block) => ???
    // schemeCall("state", nameDef(TState.interface), nameDef(TState.get), nameDef(TState.put), toDoc(init, false), toDoc(block))

    case other => super.toDoc(s, toplevel)
  }

  def toDoc(a: Scope)(implicit C: Context): Doc = a match {
    case Here() => "here"
    case Nested(scopes) =>
      val ss: List[Doc] = scopes.map(a => toDoc(a))
      schemeCall("nested", ss)
    case ScopeVar(id) => nameRef(id)
  }
}

// this is a copy of ChezSchemeBase -- we can recover sharing
// by adding a ChezSchemeTree
trait ChezSchemeLiftedBase extends ChezSchemePrinterUtils {

  import kiama.output.PrettyPrinterTypes.Document

  def format(t: ModuleDecl)(implicit C: Context): Document =
    pretty(module(t))

  def module(m: ModuleDecl)(implicit C: Context): Doc = toDoc(m)

  def toDoc(m: ModuleDecl)(implicit C: Context): Doc =
    toDoc(m.defs, true)

  def toDoc(b: Block)(implicit C: Context): Doc

  def toDoc(p: Param)(implicit C: Context): Doc = link(p, nameDef(p.id))

  def toDoc(n: Name)(implicit C: Context): Doc = link(n, n.toString)

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

    case Run(s)     => "(run " <> toDocInBlock(s) <> ")"
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

    case Let(id, tpe, binding, body) if toplevel =>
      defineValue(nameDef(id), toDoc(binding)) <> line <> toDoc(body, toplevel)

    case Let(id, tpe, binding, body) =>
      parens("let" <+> parens(brackets(nameDef(id) <+> toDoc(binding))) <+> group(nest(line <> toDoc(body, false))))

    // do not return on the toplevel
    case Ret(e) if toplevel => ""

    case Ret(e)             => toDoc(e)

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
      }
      schemeCall("pattern-match", toDoc(sc) :: parens(vsep(clauses, line)) :: Nil)

    case other => sys error s"Can't print: ${other}"
  }

  def toDoc(p: Pattern)(implicit C: Context): Doc = p match {
    case IgnorePattern()    => "ignore"
    case AnyPattern()       => "any"
    case LiteralPattern(l)  => schemeCall("literal", toDoc(l))
    case TagPattern(id, ps) => schemeCall("match-" <> nameDef(id), ps map { p => toDoc(p) })
  }

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
