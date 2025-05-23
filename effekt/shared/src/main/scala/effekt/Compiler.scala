package effekt

import effekt.PhaseResult.{AllTransformed, CoreTransformed}
import effekt.context.Context
import effekt.core.Transformer
import effekt.namer.Namer
import effekt.source.{AnnotateCaptures, ExplicitCapabilities, ModuleDecl, ResolveExternDefs}
import effekt.symbols.Module
import effekt.typer.{BoxUnboxInference, Typer, Wellformedness}
import effekt.util.messages.{CompilerPanic, FatalPhaseError}
import effekt.util.{SourceTask, Task, VirtualSource, paths}
import kiama.output.PrettyPrinterTypes.Document
import kiama.util.{Positions, Source}

import scala.language.postfixOps

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
   * The result of [[effekt.generator.Backend]], consisting of a mapping from filename to output to be written.
   */
  case Compiled(source: Source, mainFile: String, outputFiles: Map[String, Document])
}
export PhaseResult.*

enum Stage { case Core; case Machine; case Target; }

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
 *
 * Backends that implement this interface:
 *
 * - [[generator.js.JavaScript]]
 * - [[generator.chez.ChezScheme]] (in three variants)
 * - [[generator.llvm.LLVM]]
 *
 * @tparam Executable information of this compilation run, which is passed to
 *                 the corresponding backend runner (e.g. the name of the main file)
 */
trait Compiler[Executable] {

  // The Compiler Interface:
  // -----------------------
  // Used by Driver, Server, Repl and Web

  def extension: String

  def supportedFeatureFlags: List[String]

  /**
   * Used by LSP server (Intelligence) to map positions to source trees
   */
  def getAST(source: Source)(using Context): Option[ModuleDecl] =
    ParserUntilTyper(source).map { res => res.tree }

  /**
   * This is the second-most important entry-point besides [[Driver.compileSource]].
   * It is used by
   * - Namer to resolve dependencies
   * - Server / Driver to typecheck and report type errors in VSCode
   */
  def runFrontend(source: Source)(using C: Context): Option[Module] =
    def getStackTrace(e: Throwable): String =
      val stringWriter = new java.io.StringWriter()
      e.printStackTrace(new java.io.PrintWriter(stringWriter))
      stringWriter.toString

    try {
      val res = Frontend(source).map { res =>
        val mod = res.mod
        validate(source, mod)
        mod
      }
      if C.messaging.hasErrors then None else res
    } catch {
      case FatalPhaseError(msg) =>
        C.report(msg)
        None
      case e @ CompilerPanic(msg) =>
        C.report(msg)
        C.info(getStackTrace(e))
        None
      case e =>
        C.info("Effekt Compiler Crash: " + e.toString)
        C.info(getStackTrace(e))
        None
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
  def compile(source: Source)(using Context): Option[(Map[String, String], Executable)]


  // The Compiler Compiler Phases:
  // -----------------------------
  // Components that can be used by individual (backend) implementations to structure
  // the (individual) full compiler pipeline.

  /**
   * @note The result of the phases Parser until Typer needs to be cached.
   *
   *       [[Intelligence]] uses both the results of [[getAST]] and [[runFrontend]].
   *       Since we associate trees and symbols by the *object identity* of the tree,
   *       running these phases multiple times on the same input results in different trees.
   *       In consequence, the symbols can't be found anymore. To avoid this, we
   *       use a separate task for parsing.
   *
   *       Note that it is not sufficient to just cache the Parser phase because BoxUnboxInference
   *       will rewrite the tree, meaning some nodes change their original object identity.
   */
  val ParserUntilTyper = Phase.cached("parser-until-typer") {
    /**
     * Parses a file to a syntax tree
     * [[Source]] --> [[Parsed]]
     */
    Parser andThen
      /**
       * Performs name analysis and associates Id-trees with symbols
       * [[Parsed]] --> [[NameResolved]]
       */
      Namer andThen
      /**
       * Explicit box transformation
       * [[NameResolved]] --> [[NameResolved]]
       */
      BoxUnboxInference andThen
      /**
       * Wellformedness checks (exhaustivity, non-escape)
       * [[Typechecked]] --> [[Typechecked]]
       */
      Typer
  }

  /**
   * Frontend
   */
  val Frontend = Phase.cached("frontend") {
    ParserUntilTyper andThen
      /**
       * Wellformedness checks (exhaustivity, non-escape)
       *   [[Typechecked]] --> [[Typechecked]]
       */
      Wellformedness andThen
      /**
       * Resolves `extern`s for the current backend
       * [[Typechecked]] --> [[Typechecked]]
       */
      ResolveExternDefs andThen
      /**
       * Uses annotated effects to translate to explicit capability passing
       * [[Typechecked]] --> [[Typechecked]]
       */
      ExplicitCapabilities andThen
      /**
       * Computes and annotates the capture of each subexpression
       * [[Typechecked]] --> [[Typechecked]]
       */
      AnnotateCaptures
  }

  /**
   * Middleend
   */
  val Middleend = Phase.cached("middleend", cacheBy = (in: Typechecked) => paths.lastModified(in.source)) {
    /**
     * Translates a source program to a core program
     * [[Typechecked]] --> [[CoreTransformed]]
     */
    Transformer
  }

  /**
   * Maps the phase over all core modules (dependencies and the main module)
   */
  def all(phase: Phase[CoreTransformed, CoreTransformed]): Phase[AllTransformed, AllTransformed] =
    new Phase[AllTransformed, AllTransformed] {
      val phaseName = s"all-${phase.phaseName}"

      def run(input: AllTransformed)(using Context) = for {
        main <- phase(input.main)
        dependencies <- input.dependencies.foldRight[Option[List[CoreTransformed]]](Some(Nil)) {
          case (dep, Some(deps)) => phase(dep).map(_ :: deps)
          case (_, _) => None
        }
      } yield AllTransformed(input.source, main, dependencies)
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

  lazy val Aggregate = Phase[AllTransformed, CoreTransformed]("aggregate") {
    case AllTransformed(_, CoreTransformed(src, tree, mod, main), deps) =>
      val dependencies = deps.map(d => d.core)

      // collect all information
      var declarations: List[core.Declaration] = Nil
      var externs: List[core.Extern] = Nil
      var definitions: List[core.Toplevel] = Nil
      var exports: List[symbols.Symbol] = Nil

      (dependencies :+ main).foreach { module =>
        externs ++= module.externs
        declarations ++= module.declarations
        definitions ++= module.definitions
        exports ++= module.exports
      }

      val aggregated = core.ModuleDecl(main.path, Nil, declarations, externs, definitions, exports)

      // TODO in the future check for duplicate exports
      CoreTransformed(src, tree, mod, aggregated)
  }

  lazy val Machine = Phase("machine") {
    case CoreTransformed(source, tree, mod, core) =>
      val main = Context.checkMain(mod)
      val program = machine.Transformer.transform(main, core)
      (mod, main, program)
  }

  // Helpers
  // -------
  import effekt.util.paths.file

  /**
   * Path relative to the output folder
   */
  def path(m: symbols.Module)(using C: Context): String =
    (m.path.split('/').last.replace('-', '_')) + extension
}
