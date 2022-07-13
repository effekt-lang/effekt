package effekt
package generator

import effekt.{ CompilationUnit, CoreTransformed, Phase }
import effekt.context.Context
import effekt.core.*
import effekt.symbols.Module
import effekt.symbols.{ Name, Symbol, Wildcard }
import effekt.symbols.builtins.TState
import kiama.output.ParenPrettyPrinter
import kiama.output.PrettyPrinterTypes.Document
import kiama.util.Source
import effekt.context.assertions.*

import scala.language.implicitConversions
import effekt.util.paths.*

/**
 * It would be nice if Core could have an Effect Declaration or
 * translate effect declarations to Records...
 */
object ChezSchemeCallCC extends Backend {

  /**
   * Returns [[Compiled]], containing the files that should be written to.
   */
  def compileWhole(main: CoreTransformed, dependencies: List[CoreTransformed])(using C: Context) = {
    C.checkMain(main.mod)
    val deps = dependencies.map { dep => compile(dep) }
    val result = ChezSchemeCallCCPrinter.compilationUnit(main.mod, main.core, deps)
    val mainFile = path(main.mod)
    Some(Compiled(mainFile, Map(mainFile -> result)))
  }

  /**
   * Entrypoint used by the LSP server to show the compiled output
   */
  def compileSeparate(input: CoreTransformed)(using C: Context) =
    C.using(module = input.mod) { Some(compile(input)) }

  /**
   * Compiles only the given module, does not compile dependencies
   */
  private def compile(in: CoreTransformed)(using Context): Document =
    ChezSchemeCallCCPrinter.format(in.core)

  def path(m: Module)(using C: Context): String =
    (C.config.outputPath() / m.path.replace('/', '_')).unixPath + ".ss"
}

// We can again share more code, once we added a separate tree representation
// for chez scheme code.
object ChezSchemeCallCCPrinter extends ChezSchemeBase {

  def compilationUnit(mod: Module, core: ModuleDecl, dependencies: List[Document])(using Context): Document =
    pretty {

      val main = mod.terms("main").toList.head

      prelude <>
        "(let () " <+> emptyline <>
        defineStateAccessors() <>
        vsep(dependencies.map { m => string(m.layout) }) <>
        module(core) <> emptyline <>
        defineValue("main", nameDef(main)) <> emptyline <>
        "(run " <> nameRef(main) <> "))"
    }

  override def toDoc(b: Block)(using Context): Doc = link(b, b match {
    case BlockVar(v) =>
      nameRef(v)
    case BlockLit(ps, body) =>
      schemeLambda(ps map toDoc, toDoc(body, false))
    case Member(b, id) =>
      schemeCall(nameRef(id), toDoc(b))
    case Extern(ps, body) =>
      schemeLambda(ps map toDoc, body)
    case Unbox(e) => toDoc(e)
  })

  override def toDoc(s: Stmt, toplevel: Boolean)(using Context): Doc = s match {
    case State(id, init, reg, block) => ???
      // schemeCall("state", toDoc(init, false), toDoc(block))

    // funnily enough, in callcc, we actually need to wrap toplevel definitions into run
    // pure function calls (that internally use control effects, handled away) still need to
    // be run.
    case Let(id, tpe, binding, body) if toplevel =>
      defineValue(nameDef(id), "(run (thunk " <> toDoc(binding) <> "))") <> line <> toDoc(body, toplevel)

    case other => super.toDoc(s, toplevel)
  }

  override def toDoc(e: Expr)(using Context): Doc = e match {
    case Run(s) => toDocInBlock(s)
    case other  => super.toDoc(other)
  }

  override def requiresBlock(s: Stmt): Boolean = s match {
    // this is only true for the direct-style PP
    case _: Val => true
    case _      => super.requiresBlock(s)
  }
}

