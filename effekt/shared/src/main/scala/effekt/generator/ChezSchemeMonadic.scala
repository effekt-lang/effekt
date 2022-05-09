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

/**
 * It would be nice if Core could have an Effect Declaration or
 * translate effect declarations to Records...
 */
object ChezSchemeMonadic extends Backend {

  /**
   * Returns [[Compiled]], containing the files that should be written to.
   */
  def compileWhole(main: CoreTransformed, dependencies: List[CoreTransformed])(implicit C: Context) = {
    val deps = dependencies.map { dep => compile(dep) }
    val res = ChezSchemeMonadicPrinter.compilationUnit(main.mod, main.core, deps)
    val mainFile = path(main.mod)
    Some(Compiled(mainFile, Map(mainFile -> res)))
  }

  /**
   * Entrypoint used by the LSP server to show the compiled output
   */
  def compileSeparate(input: CoreTransformed)(implicit C: Context) =
    C.using(module = input.mod) { Some(compile(input)) }

  /**
   * Compiles only the given module, does not compile dependencies
   */
  private def compile(in: CoreTransformed)(implicit C: Context): Document =
    ChezSchemeMonadicPrinter.format(in.core)

  /**
   * This is used for both: writing the files to and generating the `require` statements.
   */
  def path(m: Module)(implicit C: Context): String =
    (C.config.outputPath() / m.path.replace('/', '_')).unixPath + ".ss"
}

object ChezSchemeMonadicPrinter extends ChezSchemeBase {

  def compilationUnit(mod: Module, core: ModuleDecl, dependencies: List[Document])(implicit C: Context): Document =
    pretty {

      val main = mod.terms("main").toList.head

      prelude <>
        "(let () " <+> emptyline <>
        vsep(dependencies.map { m => string(m.layout) }) <>
        module(core) <> emptyline <>
        defineValue("main", nameDef(main)) <> emptyline <>
        "(run (" <> nameRef(main) <> ")))"
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
    case Unbox(e) => toDoc(e)
  })

  override def toDoc(s: Stmt, toplevel: Boolean)(implicit C: Context): Doc = s match {
    case Val(Wildcard(_), tpe, binding, body) if toplevel =>
      "(run " <> toDoc(binding, false) <> ")" <> emptyline <> toDoc(body, toplevel)

    case Val(id, tpe, binding, body) if toplevel =>
      defineValue(nameDef(id), "(run " <> toDoc(binding, false) <> ")") <> emptyline <> toDoc(body, toplevel)

    case Val(Wildcard(_), tpe, binding, body) =>
      schemeCall("then", toDoc(binding, false), "_", toDoc(body, false))

    case Val(id, tpe, binding, body) =>
      schemeCall("then", toDoc(binding, false), nameDef(id), toDoc(body, false))

    case Let(id, tpe, binding, body) if toplevel =>
      defineValue(nameDef(id), toDoc(binding)) <> line <> toDoc(body, toplevel)

    case Let(id, tpe, binding, body) =>
      parens("let" <+> parens(brackets(nameDef(id) <+> toDoc(binding))) <> group(nest(line <> toDoc(body, toplevel))))

    // do not return on the toplevel
    case Ret(e) if toplevel => ""

    case Ret(e)             => schemeCall("pure", List(toDoc(e)))

    case State(eff, tpe, get, put, init, block) =>
      schemeCall("state", nameDef(eff), nameDef(get), nameDef(put), toDoc(init, false), toDoc(block))

    case other => super.toDoc(s, toplevel)
  }
}
