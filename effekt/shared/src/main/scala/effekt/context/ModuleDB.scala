package effekt
package context

import effekt.symbols._
import kiama.util.Source

/**
 * The ModuleDB depends on three things:
 * - method `contentsOf` to resolve FFI includes (js files)
 * - method `findSource` to resolve module sources (effekt files)
 * - field `Compiler.frontend` to run the compiler on sources, on demand
 */
trait ModuleDB { self: Context =>

  /**
   * Tries to find a file in the workspace, that matches the import path
   *
   * Used by Namer to resolve FFI includes
   */
  //  def contentsOf: Task[String, String]

  def contentsOf(path: String): Option[String]

  /**
   * Find the source for a given module name / path
   */
  private[context] def findSource(path: String): Option[Source]

  /**
   * Tries to find a module for the given path, will run compiler on demand
   *
   * Used by Namer and Evaluator to resolve imports
   */
  def moduleOf(path: String): Module =
    moduleOf(findSource(path).getOrElse { abort(s"Cannot find source for $path") })

  /**
   * Tries to find a module for the given source, will run compiler on demand
   */
  def moduleOf(source: Source): Module =
    tryModuleOf(source).getOrElse {
      abort(s"Cannot compile dependency: ${stripSuffix(source.name)}")
    }

  private def stripSuffix(path: String): String =
    path.stripSuffix(".md").stripSuffix(".effekt")

  /**
   * Tries to find a module for the given source, will run compiler on demand
   */
  def tryModuleOf(source: Source): Option[Module] = for {
    mod <- compiler.runFrontend(source)(using this)
  } yield mod

  /**
   * Util to check whether main exists on the given module
   */
  def checkMain(mod: Module)(implicit C: Context): TermSymbol = C.at(mod.decl) {

    // deep discovery of main: should be replaced by explicit @main annotation
    def findMain(b: Bindings): Set[TermSymbol] =
      b.terms.getOrElse("main", Set()) ++ b.namespaces.flatMap { case (_, b) => findMain(b) }

    val mains = findMain(mod.exports)

    if (mains.isEmpty) {
      C.abort("No main function defined")
    }

    if (mains.size > 1) {
      val names = mains.toList.map(sym => pp"${sym.name}").mkString(", ")
      C.abort(pp"Multiple main functions defined: ${names}")
    }

    val main = mains.head

    val mainValueParams = C.functionTypeOf(main).vparams
    val mainBlockParams = C.functionTypeOf(main).bparams
    if (mainValueParams.nonEmpty || mainBlockParams.nonEmpty) {
      C.abort("Main does not take arguments")
    }

    val tpe = C.functionTypeOf(main)
    val controlEffects = tpe.effects
    if (controlEffects.nonEmpty) {
      C.abort(pp"Main cannot have user defined effects, but includes effects: ${controlEffects}")
    }

    main
  }
}
