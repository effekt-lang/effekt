package effekt

import effekt.PhaseResult.{ AllTransformed, Compiled }
import effekt.context.Context
import effekt.core.DirectStyleMutableState
import effekt.util.paths.File
import effekt.symbols.Module
import effekt.source.ModuleDecl

import kiama.output.PrettyPrinterTypes.Document
import kiama.util.Source

// TODO, could be backend specific in the future
//  not all of them need to be supported by each backend
enum Stage {
  case Core
  case Lifted
  case Machine
  case Target
}

/**
 * ...
 *
 * Every single method can use the context to report errors or throw
 * exceptions.
 *
 * @tparam Executable information of this compilation run, which is passed to
 *                 the corresponding backend runner (e.g. the name of the main file)
 */
trait BackendCompiler[Executable] extends Compiler[Executable] {

  /**
   * Used by LSP server (Intelligence) to map positions to source trees
   */
  def getAST(source: Source)(using Context): Option[ModuleDecl] =
    CachedParser(source).map { res => res.tree }

  /**
   * Used by
   * - Namer to resolve dependencies
   * - Server / Driver to typecheck and report type errors in VSCode
   */
  def runFrontend(source: Source)(using Context): Option[Module] =
    Frontend(source).map { res =>
      val mod = res.mod
      validate(source, mod)
      mod
    }

  /**
   * Called after running the frontend from editor services.
   *
   * Can be overridden to implement backend specific checks (exclude certain
   * syntactic constructs, etc.)
   */
  def validate(source: Source, mod: Module)(using Context): Unit = ()

  /**
   * Show the IR of this backend for [[source]] after [[stage]].
   * Backends can return [[None]] if they do not support this.
   *
   * Used to show the IR in VSCode. For the JS Backend, also used for
   * separate compilation in the web.
   */
  def prettyIR(source: Source, stage: Stage)(using Context): Option[Document]

  /**
   * Return the backend specific AST for [[source]] after [[stage]].
   * Backends can return [[None]] if they do not support this.
   *
   * The AST will be pretty printed with a generic [[effekt.util.PrettyPrinter]].
   */
  def treeIR(source: Source, stage: Stage)(using Context): Option[Any]

  /**
   * Should compile [[source]] with this backend. Each backend can
   * choose the representation of the executable.
   */
  def compile(source: Source)(using Context): Option[(Map[String, Document], Executable)]

  /**
   * Should compile [[source]] with this backend, the compilation result should only include
   * the contents of this file, not its dependencies. Only used by the website and implemented
   * by the JS backend. All other backends can return `None`.
   */
  def compileSeparate(source: Source)(using Context): Option[Document] = None
}



object JSCompiler extends BackendCompiler[String] {

  import effekt.generator.js
  import effekt.generator.js.JavaScript

  // The Different Phases:
  // ---------------------

  val Core = Phase.cached("to-core") {
    Frontend andThen Middleend andThen DirectStyleMutableState
  }
  object Whole extends Phase[CoreTransformed, Compiled] {
    val phaseName = "javascript"

    def run(in: CoreTransformed)(using C: Context) =
      val mainSymbol = C.checkMain(in.mod)
      JavaScript.compileWhole(in, mainSymbol)
  }

  object Separate extends Phase[AllTransformed, Document] {
    val phaseName = "javascript"

    def run(in: AllTransformed)(using Context) =
      JavaScript.compileSeparate(in)
  }

  val CompileSeparate = allToCore(Core) andThen Separate
  val CompileWhole = allToCore(Core) andThen Aggregate andThen Whole


  override def prettyIR(source: Source, stage: Stage)(using Context): Option[Document] = stage match {
    case Stage.Core => Core(source).map { res => core.PrettyPrinter.format(res.core) }
    case Stage.Lifted => None
    case Stage.Machine => None
    case Stage.Target => CompileSeparate(source)
  }

  override def treeIR(source: Source, stage: Stage)(using Context): Option[Any] = stage match {
    case Stage.Core => Core(source).map { res => res.core }
    case Stage.Lifted => None
    case Stage.Machine => None
    case Stage.Target => None
  }

  override def compile(source: Source)(using C: Context) = CompileWhole(source).map {
    case Compiled(source, main, out) => (out, main)
  }

  override def compileSeparate(source: Source)(using Context): Option[Document] =
    CompileSeparate(source)
}
