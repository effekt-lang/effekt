package effekt

import effekt.context.Context
import effekt.core.Transformer
import effekt.lifted.LiftInference
import effekt.namer.Namer
import effekt.regions.RegionChecker
import effekt.source.{ CapabilityPassing, ModuleDecl }
import effekt.symbols.Module
import effekt.typer.Typer
import effekt.util.messages.FatalPhaseError
import effekt.util.{ SourceTask, Task, VirtualSource, paths }
import effekt.generator.Backend

import kiama.output.PrettyPrinterTypes.Document
import kiama.util.{ Positions, Source }

/**
 * The compiler for the Effekt language.
 *
 * The compiler is set up in the following large phases that consist itself of potentially multiple phases
 *
 * (1) Parser    (Source      -> source.Tree)  Load file and parse it into an AST
 *
 * (2) Frontend  (source.Tree -> source.Tree)  Perform name analysis, typechecking, region inference,
 *                                             and other rewriting of the source AST
 *
 * (3) Middleend (source.Tree -> core.Tree)    Perform an ANF transformation into core, and
 *                                             other rewritings on the core AST
 *
 * (4) Backend  (core.Tree   -> Document)     Generate code in a target language
 *
 * The compiler itself does not read from or write to files. This is important since, we need to
 * virtualize the file system to also run the compiler in the browser.
 *
 * - Reading files is performed by the mixin [[effekt.context.ModuleDB]], which is implemented
 *   differently for the JS and JVM versions.
 * - Writing to files is performed by the hook [[Compiler.saveOutput]], which is implemented
 *   differently for the JS and JVM versions.
 */

/**
 * Intermediate results produced by the various phases.
 *
 * All phases have a source field, which is mostly used to invalidate caches based on the timestamp.
 */
sealed trait PhaseResult { val source: Source }

/**
 * The result of [[Parser]] parsing a single file into a [[effekt.source.Tree]].
 */
case class Parsed(source: Source, tree: ModuleDecl) extends PhaseResult

/**
 * The result of [[Namer]] resolving all names in a given syntax tree. The resolved symbols are
 * annotated in the [[Context]] using [[effekt.context.Annotations]].
 */
case class NameResolved(source: Source, tree: ModuleDecl, mod: symbols.Module) extends PhaseResult

/**
 * The result of [[Typer]] type checking a given syntax tree.
 *
 * We can notice that [[NameResolved]] and [[Typechecked]] haave the same fields.
 * Like, [[Namer]], [[Typer]] writes to the types of each tree into the DB, using [[effekt.context.Annotations]].
 * This might change in the future, when we switch to elaboration.
 */
case class Typechecked(source: Source, tree: ModuleDecl, mod: symbols.Module) extends PhaseResult

/**
 * The result of [[Transformer]] ANF transforming [[source.Tree]] into the core representation [[core.Tree]].
 */
case class CoreTransformed(source: Source, tree: ModuleDecl, mod: symbols.Module, core: effekt.core.ModuleDecl) extends PhaseResult

/**
 * The result of [[LiftInference]] transforming [[core.Tree]] into the lifted core representation [[lifted.Tree]].
 */
case class CoreLifted(source: Source, tree: ModuleDecl, mod: symbols.Module, core: effekt.lifted.ModuleDecl) extends PhaseResult

/**
 * The result of [[LowerDependencies]] running all phases up to (including) ANF transformation on
 * all dependencies.
 *
 * A compilation unit consisting of all transitive dependencies in topological ordering.
 */
case class CompilationUnit(main: CoreTransformed, dependencies: List[CoreTransformed]) extends PhaseResult {
  val source = main.source
}

/**
 * The result of [[effekt.generator.Backend]], consisting of a mapping from filename to output to be written.
 */
case class Compiled(mainFile: String, outputFiles: Map[String, Document])

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
       * Type checks and annotates trees with inferred types and effects
       *   [[NameResolved]] --> [[Typechecked]]
       */
      Typer andThen
      /**
       * Uses annotated effects to translate to explicit capability passing
       *   [[Typechecked]] --> [[Typechecked]]
       */
      CapabilityPassing
  }

  /**
   * Middleend
   */
  private val Middleend = Phase.cached("middleend", cacheBy = (in: Typechecked) => paths.lastModified(in.source)) {
    Transformer
    // here optimization passes on Core will be added.
  }

  /**
   * Backend
   */
  def Backend(implicit C: Context): Backend = C.config.backend() match {
    case "js"           => effekt.generator.JavaScriptMonadic
    case "chez-callcc"  => effekt.generator.ChezSchemeCallCC
    case "chez-monadic" => effekt.generator.ChezSchemeMonadic
    case "chez-lift"    => effekt.generator.ChezSchemeLift
  }

  object LowerDependencies extends Phase[CoreTransformed, CompilationUnit] {
    val phaseName = "lower-dependencies"
    def run(main: CoreTransformed)(implicit C: Context) = {
      val dependencies = main.mod.dependencies flatMap { dep =>
        // We already ran the frontend on the dependencies (since they are discovered
        // dynamically). The results are cached, so it doesn't hurt dramatically to run
        // the frontend again. However, the indirection via dep.source is not very elegant.
        (Frontend andThen Middleend)(dep.source)
      }
      Some(CompilationUnit(main, dependencies))
    }
  }

  // Compiler Interface
  // ==================
  // As it is used by other parts of the language implementation.
  // All methods return Option, the errors are reported using the compiler context [[Context]].

  /**
   * Used by LSP server (Intelligence) to map positions to source trees
   */
  def getAST(source: Source)(implicit C: Context): Option[ModuleDecl] =
    CachedParser(source).map { res => res.tree }

  /**
   * Called by ModuleDB (and indirectly by Namer) to run the frontend for a
   * dependency.
   */
  def runFrontend(source: Source)(implicit C: Context): Option[Module] =
    Frontend(source).map { res => res.mod }

  /**
   * This is used from the JS implementation ([[effekt.LanguageServer]]) and also
   * from the LSP server ([[effekt.Server]]) after compilation.
   *
   * It does **not** generate files and write them using `saveOutput`!
   * This is achieved by `compileWhole`.
   *
   * TODO Currently the backend is not cached at all
   */
  def compileSeparate(source: Source)(implicit C: Context): Option[(CoreTransformed, Document)] =
    (Frontend andThen Middleend andThen Backend.separate).apply(source)

  /**
   * Used by [[Driver]] and by [[Repl]] to compile a file
   */
  def compileWhole(source: Source)(implicit C: Context): Option[Compiled] =
    (Frontend andThen Middleend andThen LowerDependencies andThen Backend.whole).apply(source)
}
