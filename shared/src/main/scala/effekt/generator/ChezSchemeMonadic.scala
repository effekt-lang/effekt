package effekt.generator

import effekt.context.Context
import effekt.core._
import effekt.symbols.Module
import effekt.symbols.{ Name, Symbol, Wildcard }

import org.bitbucket.inkytonik.kiama
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
class ChezSchemeMonadic extends Generator {

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
    core <- C.lower(src)
    result = ChezSchemeMonadicPrinter.compilationUnit(mod, core, deps)
    _ = C.saveOutput(result.layout, path(mod))
  } yield result

  /**
   * Compiles only the given module, does not compile dependencies
   */
  def compile(mod: Module)(implicit C: Context): Option[Document] = for {
    core <- C.lower(mod.source)
    doc = ChezSchemeMonadicPrinter.format(core)
  } yield doc
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
    case _ => sys error "Lifts not supported"
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
