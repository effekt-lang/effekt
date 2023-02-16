package effekt

import effekt.PhaseResult.{ CoreLifted, CoreTransformed }
import effekt.context.Context
import effekt.core.{ DirectStyleMutableState, Transformer }
import effekt.lifted.LiftInference
import effekt.namer.Namer
import effekt.source.{ Elaborator, ModuleDecl }
import effekt.symbols.Module
import effekt.typer.{ PostTyper, PreTyper, Typer }
import effekt.util.messages.FatalPhaseError
import effekt.util.{ SourceTask, Task, VirtualSource, paths }
import effekt.generator.Backend
import effekt.generator.js.JavaScript
import kiama.output.PrettyPrinterTypes.Document
import kiama.util.{ Positions, Source }

/**
 * Intermediate results produced by the various phases.
 *
 * All phases have a source field, which is mostly used to invalidate caches based on the timestamp.
 */
enum PhaseResult {

  val source: Source

  /**
   * The result of [[Parser]] parsing a single file into a [[effekt.source.Tree]].
   */
  case Parsed(source: Source, tree: ModuleDecl)

  /**
   * The result of [[Namer]] resolving all names in a given syntax tree. The resolved symbols are
   * annotated in the [[Context]] using [[effekt.context.Annotations]].
   */
  case NameResolved(source: Source, tree: ModuleDecl, mod: symbols.Module)

  /**
   * The result of [[Typer]] type checking a given syntax tree.
   *
   * We can notice that [[NameResolved]] and [[Typechecked]] haave the same fields.
   * Like, [[Namer]], [[Typer]] writes to the types of each tree into the DB, using [[effekt.context.Annotations]].
   * This might change in the future, when we switch to elaboration.
   */
  case Typechecked(source: Source, tree: ModuleDecl, mod: symbols.Module)

  /**
   * The result of [[Transformer]] ANF transforming [[source.Tree]] into the core representation [[core.Tree]].
   */
  case CoreTransformed(source: Source, tree: ModuleDecl, mod: symbols.Module, core: effekt.core.ModuleDecl)

  /**
   * The result of running the [[Compiler.Middleend]] on all dependencies.
   */
  case AllTransformed(source: Source, main: PhaseResult.CoreTransformed, dependencies: List[PhaseResult.CoreTransformed])

  /**
   * The result of [[LiftInference]] transforming [[core.Tree]] into the lifted core representation [[lifted.Tree]].
   */
  case CoreLifted(source: Source, tree: ModuleDecl, mod: symbols.Module, core: effekt.lifted.ModuleDecl)

  /**
   * The result of [[effekt.generator.Backend]], consisting of a mapping from filename to output to be written.
   */
  case Compiled(source: Source, mainFile: String, outputFiles: Map[String, Document])
}
export PhaseResult.*

/**
 * The compiler for the Effekt language.
 *
 * The compiler is set up in the following large phases that consist itself of potentially multiple phases
 *
 *   1. Parser    (Source      -> source.Tree)  Load file and parse it into an AST
 *
 *   2. Frontend  (source.Tree -> source.Tree)  Perform name analysis, typechecking, region inference,
 *                                             and other rewriting of the source AST
 *
 *   3. Middleend (source.Tree -> core.Tree)    Perform an ANF transformation into core, and
 *                                             other rewritings on the core AST
 *
 *   4. Backend  (core.Tree   -> Document)     Generate code in a target language
 *
 * The compiler itself does not read from or write to files. This is important since, we need to
 * virtualize the file system to also run the compiler in the browser.
 *
 * - Reading files is performed by the mixin [[effekt.context.ModuleDB]], which is implemented
 *   differently for the JS and JVM versions.
 * - Writing to files is performed by the hook [[Compiler.saveOutput]], which is implemented
 *   differently for the JS and JVM versions.
 */
trait Compiler {

  /**
   * @note The result of parsing needs to be cached.
   *
   *       [[Intelligence]] uses both the results of [[getAST]] and [[runFrontend]].
   *       Since we associate trees and symbols by the *object identity* of the tree,
   *       running parser multiple times on the same input results in different trees.
   *       In consequence, the symbols can't be found anymore. To avoid this, we
   *       use a separate task for parsing.
   *
   *       Having access to the parse trees separately is also helpful for programs
   *       that fail in later phases (for instance type checking). This way some
   *       editor services can be provided, even in presence of errors.
   */
  private val CachedParser = Phase.cached("cached-parser") { Parser }

  /**
   * Frontend
   */
  private val Frontend = Phase.cached("frontend") {
    /**
     * Parses a file to a syntax tree
     *   [[Source]] --> [[Parsed]]
     */
    CachedParser andThen
      /**
       * Performs name analysis and associates Id-trees with symbols
       *    [[Parsed]] --> [[NameResolved]]
       */
      Namer andThen
      /**
       * Explicit box transformation
       *   [[NameResolved]] --> [[NameResolved]]
       */
      PreTyper andThen
      /**
       * Type checks and annotates trees with inferred types and effects
       *   [[NameResolved]] --> [[Typechecked]]
       */
      Typer andThen
      /**
       * Wellformedness checks (exhaustivity, non-escape)
       *   [[Typechecked]] --> [[Typechecked]]
       */
      PostTyper
  }

  /**
   * Middleend
   */
  val Middleend = Phase.cached("middleend", cacheBy = (in: Typechecked) => paths.lastModified(in.source)) {
    /**
     * Uses annotated effects to translate to explicit capability passing
     * [[Typechecked]] --> [[Typechecked]]
     */
    Elaborator andThen
    /**
     * Translates a source program to a core program
     * [[Typechecked]] --> [[CoreTransformed]]
     */
    Transformer
  }

  /**
   * Backend
   */
  def Backend(using C: Context): Backend = C.config.backend() match {
    case "js"           => effekt.generator.js.JavaScript
    case "chez-callcc"  => effekt.generator.chez.ChezSchemeCallCC
    case "chez-monadic" => effekt.generator.chez.ChezSchemeMonadic
    case "chez-lift"    => effekt.generator.chez.ChezSchemeLift
    case "llvm"         => effekt.generator.llvm.LLVM
    case "ml"           => effekt.generator.ml.ML
  }

  object CoreDependencies extends Phase[CoreTransformed, AllTransformed] {
    val phaseName = "core-dependencies"

    def run(input: CoreTransformed)(using Context) = {
      val CoreTransformed(src, tree, mod, main) = input

      val dependencies = mod.dependencies flatMap { dep =>
        // We already ran the frontend on the dependencies (since they are discovered
        // dynamically). The results are cached, so it doesn't hurt dramatically to run
        // the frontend again. However, the indirection via dep.source is not very elegant.
        (Frontend andThen Middleend).apply(dep.source)
      }
      Some(AllTransformed(input.source, input, dependencies))
    }
  }

  def allToCore(phase: Phase[Source, CoreTransformed]): Phase[Source, AllTransformed] = new Phase[Source, AllTransformed] {
    val phaseName = "core-dependencies"

    def run(input: Source)(using Context) = for {
      main @ CoreTransformed(_, _, mod, _) <- phase(input)
      dependencies <- mod.dependencies.foldRight[Option[List[CoreTransformed]]](Some(Nil)) {
        case (dep, Some(deps)) => phase(dep.source).map(_ :: deps)
        case (_, _) => None
      }
    } yield AllTransformed(input, main, dependencies)
  }

  object Aggregate extends Phase[AllTransformed, CoreTransformed] {
    val phaseName = "aggregate"

    def run(input: AllTransformed)(using Context) = {
      val CoreTransformed(src, tree, mod, main) = input.main
      val dependencies = input.dependencies.map(d => d.core)

      // collect all information
      var declarations: List[core.Declaration] = Nil
      var externs: List[core.Extern] = Nil
      var definitions: List[core.Definition] = Nil
      var exports: List[symbols.Symbol] = Nil

      (dependencies :+ main).foreach { module =>
        externs ++= module.externs
        declarations ++= module.declarations
        definitions ++= module.definitions
        exports ++= module.exports
      }

      val aggregated = core.ModuleDecl(main.path, Nil, declarations, externs, definitions, exports)

      // TODO in the future check for duplicate exports
      Some(CoreTransformed(src, tree, mod, aggregated))
    }
  }

  // Compiler Interface
  // ==================
  // As it is used by other parts of the language implementation.
  // All methods return Option, the errors are reported using the compiler context [[Context]].

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
    Frontend(source).map { res => res.mod }

  /**
   * This is used from the JS implementation ([[effekt.LanguageServer]]) and also
   * from the LSP server ([[effekt.Server]]) after compilation.
   *
   * It does **not** generate files and write them using `saveOutput`!
   * This is achieved by `compileWhole`.
   */
  def compileSeparate(source: Source)(using C: Context): Option[(CoreTransformed, Document)] = C.config.backend() match {
    case "llvm" => llvm.separate(source)
    case "js"   => js.separate(source)
    case _      => (Frontend andThen Middleend andThen CoreDependencies andThen Backend.separate).apply(source)
  }

  /**
   * Used by [[Driver]] and by [[Repl]] to compile a file
   */
  def compileWhole(source: Source)(using C: Context): Option[Compiled] = C.config.backend() match {
    case "llvm" => llvm.whole(source)
    case "js"   => js.whole(source)
    case _      => (Frontend andThen Middleend andThen CoreDependencies andThen Aggregate andThen Backend.whole).apply(source)
  }

  /**
   * Used by [[Server]] to print the core tree of backends with whole-program compilation
   */
  def compileAll(source: Source)(using C: Context): Option[CoreTransformed] = C.config.backend() match {
    case "llvm" => llvm.allCore(source)
    case "js" => js.allCore(source)
    case _ => (Frontend andThen Middleend andThen CoreDependencies andThen Aggregate).apply(source)
  }

  // Different Backends
  // ==================
  object js {
    import effekt.generator.js.JavaScript

    val toCore = Phase.cached("to-core") { Frontend andThen Middleend andThen DirectStyleMutableState }
    val allCore = allToCore(toCore) andThen Aggregate
    val separate = allToCore(toCore) andThen JavaScript.separate
    val whole = allCore andThen JavaScript.whole
  }

  object llvm {
    import effekt.generator.llvm.LLVM

    val toCore = Phase.cached("to-core") { Frontend andThen Middleend andThen core.PolymorphismBoxing }
    val allCore = allToCore(toCore) andThen Aggregate

    // TODO move lift inference and machine transformations from individual backends to toplevel.
    val lifted = allCore andThen LiftInference andThen Machine

    val separate = allToCore(toCore) andThen LLVM.separate
    val whole = allCore andThen LLVM.whole
  }

  object Machine extends Phase[CoreLifted, machine.Program] {
    val phaseName = "machine"

    def run(input: CoreLifted)(using C: Context) = {
      val main = C.checkMain(input.mod);
      Some(machine.Transformer.transform(main, input.core))
    }
  }
}
