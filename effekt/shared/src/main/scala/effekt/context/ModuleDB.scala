package effekt
package context

import effekt.symbols.Module
import org.bitbucket.inkytonik.kiama.util.Source

/**
 * The ModuleDB depends on three things:
 * - method `contentsOf` to resolve FFI includes (js files)
 * - method `findSource` to resolve module sources (effekt files)
 * - field `compiler` to run the compiler on sources, on demand
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
  def moduleOf(source: Source): Module = {
    tryModuleOf(source).getOrElse {
      abort(s"Cannot compile dependency: ${source.name}")
    }
  }

  /**
   * Tries to find a module for the given source, will run compiler on demand
   */
  def tryModuleOf(source: Source): Option[Module] = for {
    mod <- frontend(source)(this)
  } yield mod

  /**
   * Util to check whether main exists on the given module
   */
  def checkMain(mod: Module)(implicit C: Context): Unit = C.at(mod.decl) {
    val mains = mod.terms.getOrElse("main", Set())

    if (mains.isEmpty) {
      C.abort("No main function defined")
    }

    if (mains.size > 1) {
      C.abort("Multiple main functions defined")
    }

    val main = mains.head

    val mainParams = C.blockTypeOf(main).params
    if ((mainParams.size != 1) || (mainParams.head != Nil)) {
      C.abort("Main does not take arguments")
    }

    val tpe = C.blockTypeOf(main)
    val userEffects = tpe.ret.effects.userDefined
    if (userEffects.nonEmpty) {
      C.abort(s"Main cannot have user defined effects, but includes effects: ${userEffects}")
    }
  }
}
