package effekt
package generator

import effekt.context.Context
import effekt.core.*
import effekt.symbols.Module
import effekt.symbols.Wildcard
import kiama.output.PrettyPrinterTypes.Document
import kiama.util.Source

import scala.language.implicitConversions
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

object ChezSchemeLiftPrinter extends ChezSchemeBase {

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

    case Ret(e) => schemeCall("pure", List(toDoc(e)))

    case Val(id, tpe, binding, body) =>
      schemeCall("then", toDoc(binding, false), nameDef(id), toDoc(body, false))

    case Def(id, tpe, ScopeAbs(sc, BlockLit(ps, body)), rest) =>
      defineFunction(nameDef(id), List(nameDef(sc)),
        schemeLambda(ps map toDoc, toDoc(body, false))) <> emptyline <> toDoc(rest, toplevel)

    case State(eff, tpe, get, put, init, block) =>
      schemeCall("state", nameDef(eff), nameDef(get), nameDef(put), toDoc(init, false), toDoc(block))

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
