package effekt

import effekt.context.Context
import effekt.core.DirectStyleMutableState
import effekt.lifted.LiftInference
import effekt.util.paths.{ File, file}
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


class JSCompiler extends BackendCompiler[String] {

  import effekt.generator.js
  import effekt.generator.js.JavaScript


  // Implementation of the Compiler Interface:
  // -----------------------------------------

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


  // The Different Phases:
  // ---------------------

  val Core = Phase.cached("core") {
    Frontend andThen Middleend andThen DirectStyleMutableState
  }
  object Whole extends Phase[CoreTransformed, Compiled] {
    val phaseName = "javascript-whole"

    def run(in: CoreTransformed)(using C: Context) =
      val mainSymbol = C.checkMain(in.mod)
      JavaScript.compileWhole(in, mainSymbol)
  }

  object Separate extends Phase[AllTransformed, Document] {
    val phaseName = "javascript-separate"

    def run(in: AllTransformed)(using Context) =
      JavaScript.compileSeparate(in)
  }

  val CompileSeparate = allToCore(Core) andThen Separate
  val CompileWhole = allToCore(Core) andThen Aggregate andThen Whole

}


class ChezMonadicCompiler extends BackendCompiler[String] {

  import effekt.generator.chez
  import effekt.generator.chez.ChezSchemeMonadic

  // Implementation of the Compiler Interface:
  // -----------------------------------------

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


  // The Different Phases:
  // ---------------------

  val Core = Phase.cached("core") {
    Frontend andThen Middleend
  }
  object Whole extends Phase[CoreTransformed, Compiled] {
    val phaseName = "chez-monadic-whole"

    def run(in: CoreTransformed)(using C: Context) =
      val mainSymbol = C.checkMain(in.mod)
      ChezSchemeMonadic.compileWhole(in, mainSymbol)
  }

  object Separate extends Phase[AllTransformed, Document] {
    val phaseName = "chez-monadic-separate"

    def run(in: AllTransformed)(using Context) =
      ChezSchemeMonadic.compileSeparate(in)
  }

  val CompileSeparate = allToCore(Core) andThen Separate
  val CompileWhole = allToCore(Core) andThen Aggregate andThen Whole
}


class ChezCallCCCompiler extends BackendCompiler[String] {

  import effekt.generator.chez
  import effekt.generator.chez.ChezSchemeCallCC

  // Implementation of the Compiler Interface:
  // -----------------------------------------

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


  // The Different Phases:
  // ---------------------

  val Core = Phase.cached("to-core") {
    Frontend andThen Middleend
  }
  object Whole extends Phase[CoreTransformed, Compiled] {
    val phaseName = "chez-callcc-whole"

    def run(in: CoreTransformed)(using C: Context) =
      val mainSymbol = C.checkMain(in.mod)
      ChezSchemeCallCC.compileWhole(in, mainSymbol)
  }

  object Separate extends Phase[AllTransformed, Document] {
    val phaseName = "chez-callcc-separate"

    def run(in: AllTransformed)(using Context) =
      ChezSchemeCallCC.compileSeparate(in)
  }

  val CompileSeparate = allToCore(Core) andThen Separate
  val CompileWhole = allToCore(Core) andThen Aggregate andThen Whole
}


class ChezLiftCompiler extends BackendCompiler[String] {

  import effekt.generator.chez
  import effekt.generator.chez.ChezSchemeLift

  // Implementation of the Compiler Interface:
  // -----------------------------------------
  override def prettyIR(source: Source, stage: Stage)(using Context): Option[Document] = stage match {
    case Stage.Core => Core(source).map { res => core.PrettyPrinter.format(res.core) }
    case Stage.Lifted => (Core andThen LiftInference)(source).map { res => lifted.PrettyPrinter.format(res.core) }
    case Stage.Machine => None
    case Stage.Target => Separate(source)
  }

  override def treeIR(source: Source, stage: Stage)(using Context): Option[Any] = stage match {
    case Stage.Core => Core(source).map { res => res.core }
    case Stage.Lifted => (Core andThen LiftInference)(source).map { res => res.core }
    case Stage.Machine => None
    case Stage.Target => None
  }

  override def compile(source: Source)(using C: Context) = Compile(source)

  override def compileSeparate(source: Source)(using Context) = Separate(source)


  // The Compilation Pipeline
  // ------------------------
  // Source => Core => Lifted => Chez
  lazy val Compile =
    allToCore(Core) andThen Aggregate andThen LiftInference andThen Chez map { case (main, expr) =>
      (Map(main -> pretty(expr)), main)
    }

  lazy val Core = Phase.cached("core") { Frontend andThen Middleend }

  lazy val Chez = Phase("chez") {
    case CoreLifted(source, tree, mod, core) =>
      val mainSymbol = Context.checkMain(mod)
      val mainFile = path(mod)
      mainFile -> chez.Let(Nil, ChezSchemeLift.toChez(mainSymbol, mod, core))
  }

  // The Compilation Pipeline for VSCode
  // -----------------------------------
  lazy val Separate =
    allToCore(Core) map { all => all.main } andThen LiftInference andThen Chez map { case (_, expr) => pretty(expr) }


  // Helpers
  // -------
  private def pretty(expr: chez.Expr): Document =
    chez.PrettyPrinter.pretty(chez.PrettyPrinter.toDoc(expr), 100)

  private def path(m: Module)(using C: Context): String =
    (C.config.outputPath() / m.path.replace('/', '_')).unixPath + ".ss"
}


class LLVMCompiler extends BackendCompiler[String] {

  import effekt.generator.llvm
  import effekt.generator.llvm.LLVM

  // Implementation of the Compiler Interface:
  // -----------------------------------------

  override def prettyIR(source: Source, stage: Stage)(using Context): Option[Document] = stage match {
    case Stage.Core => Core(source).map { res => core.PrettyPrinter.format(res.core) }
    case Stage.Lifted => (Core andThen LiftInference)(source).map { res => lifted.PrettyPrinter.format(res.core) }
    case Stage.Machine => None
    case Stage.Target => compileSeparate(source)
  }

  override def treeIR(source: Source, stage: Stage)(using Context): Option[Any] = stage match {
    case Stage.Core => Core(source).map { res => res.core }
    case Stage.Lifted => (Core andThen LiftInference)(source).map { res => res.core }
    case Stage.Machine => None
    case Stage.Target => None
  }

  override def compile(source: Source)(using C: Context) = CompileWhole(source) map {
    case Compiled(source, main, out) => (out, main)
  }

  override def compileSeparate(source: Source)(using Context): Option[Document] = CompileSeparate(source) map {
    res => res._2
  }

  // The Different Phases:
  // ---------------------

  val Core = Phase.cached("core") {
    Frontend andThen Middleend andThen core.PolymorphismBoxing
  }

  val AllCore = allToCore(Core) andThen Aggregate

  // TODO move lift inference and machine transformations from individual backends to toplevel.
  val Lifted = AllCore andThen LiftInference andThen Machine

  val CompileSeparate = allToCore(Core) andThen LLVM.separate
  val CompileWhole = allToCore(Core) andThen Aggregate andThen LLVM.whole
}


class MLCompiler extends BackendCompiler[String] {

  import effekt.generator.ml
  import effekt.generator.ml.ML

  // Implementation of the Compiler Interface:
  // -----------------------------------------

  override def prettyIR(source: Source, stage: Stage)(using Context): Option[Document] = stage match {
    case Stage.Core => Core(source).map { res => core.PrettyPrinter.format(res.core) }
    case Stage.Lifted => (Core andThen LiftInference)(source).map { res => lifted.PrettyPrinter.format(res.core) }
    case Stage.Machine => None
    case Stage.Target => compileSeparate(source)
  }

  override def treeIR(source: Source, stage: Stage)(using Context): Option[Any] = stage match {
    case Stage.Core => Core(source).map { res => res.core }
    case Stage.Lifted => (Core andThen LiftInference)(source).map { res => res.core }
    case Stage.Machine => None
    case Stage.Target => None
  }

  override def compile(source: Source)(using C: Context) = CompileWhole(source) map {
    case Compiled(source, main, out) => (out, main)
  }

  override def compileSeparate(source: Source)(using Context): Option[Document] = CompileSeparate(source) map {
    res => res._2
  }

  // The Different Phases:
  // ---------------------

  val Core = Phase.cached("core") {
    Frontend andThen Middleend
  }

  val AllCore = allToCore(Core) andThen Aggregate

  // TODO move lift inference and machine transformations from individual backends to toplevel.
  val Lifted = AllCore andThen LiftInference andThen Machine

  val CompileSeparate = allToCore(Core) andThen ML.separate
  val CompileWhole = allToCore(Core) andThen Aggregate andThen ML.whole
}




