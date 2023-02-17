package effekt

import effekt.context.Context
import effekt.core.DirectStyleMutableState
import effekt.lifted.LiftInference
import effekt.util.paths.{ File, file }
import effekt.symbols.{ Module, Symbol }
import effekt.source.ModuleDecl
import kiama.output.PrettyPrinterTypes.{ Document, emptyLinks }
import kiama.util.Source


class JSCompiler extends Compiler[String] {

  import effekt.generator.js
  import effekt.generator.js.JavaScript


  // Implementation of the Compiler Interface:
  // -----------------------------------------

  override def prettyIR(source: Source, stage: Stage)(using Context): Option[Document] = stage match {
    case Stage.Core => Core(source).map { res => core.PrettyPrinter.format(res.core) }
    case Stage.Lifted => None
    case Stage.Machine => None
    case Stage.Target => CompileSeparate(source) map { case (core, prog) => pretty(prog.virtual) }
  }

  override def treeIR(source: Source, stage: Stage)(using Context): Option[Any] = stage match {
    case Stage.Core => Core(source).map { res => res.core }
    case Stage.Lifted => None
    case Stage.Machine => None
    case Stage.Target => CompileSeparate(source) map { case (core, prog) => prog }
  }

  override def compile(source: Source)(using C: Context) = Compile(source)

  override def compileSeparate(source: Source)(using Context): Option[(CoreTransformed, Document)] =
    CompileSeparate(source).map { (core, prog) => (core, pretty(prog.virtual)) }


  // The Different Phases:
  // ---------------------

  val Core = Phase.cached("core") {
    Frontend andThen Middleend andThen DirectStyleMutableState
  }

  val Compile = allToCore(Core) andThen Aggregate map {
    case input @ CoreTransformed(source, tree, mod, core) =>
      val mainSymbol = Context.checkMain(mod)
      val mainFile = path(mod)
      val doc = pretty(JavaScript.compile(input, mainSymbol).commonjs)
      (Map(mainFile -> doc), mainFile)
  }

  val CompileSeparate = allToCore(Core) map { in => (in.main, JavaScript.compileSeparate(in)) }

  private def pretty(stmts: List[js.Stmt]): Document =
    js.PrettyPrinter.format(stmts)

  private def path(m: Module)(using C: Context): String =
    (C.config.outputPath() / JavaScript.jsModuleFile(m.path)).unixPath
}


class ChezMonadicCompiler extends Compiler[String] {

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


class ChezCallCCCompiler extends Compiler[String] {

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


class ChezLiftCompiler extends Compiler[String] {

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


class LLVMCompiler extends Compiler[String] {

  import effekt.llvm

  // Implementation of the Compiler Interface:
  // -----------------------------------------

  override def prettyIR(source: Source, stage: Stage)(using Context): Option[Document] = stage match {
    case Stage.Core => steps.afterCore(source).map { res => core.PrettyPrinter.format(res.core) }
    case Stage.Lifted => steps.afterLift(source).map { res => lifted.PrettyPrinter.format(res.core) }
    case Stage.Machine => steps.afterMachine(source).map { res => machine.PrettyPrinter.format(res.program) }
    case Stage.Target => steps.afterLLVM(source).map { res => pretty(res) }
  }

  override def treeIR(source: Source, stage: Stage)(using Context): Option[Any] = stage match {
    case Stage.Core => steps.afterCore(source).map { res => res.core }
    case Stage.Lifted => steps.afterLift(source).map { res => res.core }
    case Stage.Machine => steps.afterMachine(source).map { res => res.program }
    case Stage.Target => steps.afterLLVM(source)
  }

  override def compile(source: Source)(using C: Context) =
    Compile(source) map { (mod, defs) =>
      val mainFile = path(mod)
      (Map(mainFile -> pretty(defs)), mainFile)
    }


  // The Different Phases:
  // ---------------------

  lazy val Core = Phase.cached("core") {
    Frontend andThen Middleend andThen core.PolymorphismBoxing
  }

  lazy val Compile = allToCore(Core) andThen Aggregate andThen LiftInference andThen Machine map {
    case (mod, main, prog) => (mod, llvm.Transformer.transform(prog))
  }

  object steps {
    // intermediate steps for VSCode
    val afterCore = allToCore(Core) map { c => c.main }
    val afterLift = afterCore andThen LiftInference
    val afterMachine = afterLift andThen Machine map { case (mod, main, prog) => prog }
    val afterLLVM = afterMachine map {
      case machine.Program(decls, prog) =>
        // we don't print declarations here.
        llvm.Transformer.transform(machine.Program(Nil, prog))
    }
  }

  // Helpers
  // -------

  private def path(m: Module)(using C: Context): String =
    (C.config.outputPath() / m.path.replace('/', '_')).unixPath + ".ll"

  private def pretty(defs: List[llvm.Definition])(using Context): Document =
    Document(effekt.llvm.PrettyPrinter.show(defs), emptyLinks)
}


class MLCompiler extends Compiler[String] {

  import effekt.generator.ml
  import effekt.generator.ml.ML

  // Implementation of the Compiler Interface:
  // -----------------------------------------

  override def prettyIR(source: Source, stage: Stage)(using Context): Option[Document] = stage match {
    case Stage.Core => steps.afterCore(source).map { res => core.PrettyPrinter.format(res.core) }
    case Stage.Lifted => steps.afterLift(source).map { res => lifted.PrettyPrinter.format(res.core) }
    case Stage.Machine => None
    case Stage.Target => steps.afterML(source).map { res => pretty(res) }
  }

  override def treeIR(source: Source, stage: Stage)(using Context): Option[Any] = stage match {
    case Stage.Core => Core(source).map { res => res.core }
    case Stage.Lifted => (Core andThen LiftInference)(source).map { res => res.core }
    case Stage.Machine => None
    case Stage.Target => steps.afterML(source)
  }

  override def compile(source: Source)(using C: Context) = Compile(source)


  // The Different Phases:
  // ---------------------

  lazy val Core = Phase.cached("core") {
    Frontend andThen Middleend
  }

  lazy val ToML = Phase("ml") {
    case PhaseResult.CoreLifted(source, tree, mod, core) =>
      val mainSymbol = Context.checkMain(mod)
      val mainFile = path(mod)
      mainFile -> ml.ML.compilationUnit(mainSymbol, core)
  }

  lazy val Compile = allToCore(Core) andThen Aggregate andThen LiftInference andThen ToML map {
    case (mainFile, prog) => (Map(mainFile -> pretty(prog)), mainFile)
  }

  object steps {
    // intermediate steps for VSCode
    val afterCore = allToCore(Core) map { c => c.main }
    val afterLift = afterCore andThen LiftInference
    val afterML = afterLift andThen ToML map { case (f, prog) => prog }
  }

  def pretty(prog: ml.Toplevel) =
    ml.PrettyPrinter.format(ml.PrettyPrinter.toDoc(prog))

  def path(m: Module)(using C: Context): String =
    (C.config.outputPath() / m.path.replace('/', '_')).unixPath + ".sml"
}




