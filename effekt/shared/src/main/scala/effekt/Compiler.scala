package effekt

import effekt.context.Context
import effekt.core.{ LiftInference, Transformer }
import effekt.namer.Namer
import effekt.regions.RegionChecker
import effekt.source.{ CapabilityPassing, ModuleDecl }
import effekt.symbols.Module
import effekt.typer.Typer
import effekt.util.messages.FatalPhaseError
import effekt.util.{ SourceTask, Task, VirtualSource, paths }
import kiama.output.PrettyPrinterTypes.Document
import kiama.util.{ Positions, Source }

/**
 * A "pure" compiler without reading or writing to files.
 *
 * All methods return Option, the errors are reported in the given context
 *
 * We distinguish between Phases and Tasks. Phases, by default, perform no memoization
 * while task use the "build system" abstraction in `Task`, track dependencies and
 * avoid rebuilding by memoization. Tasks can be constructed
 *
 * The Compiler is set up in the following large tasks that consist of potentially multiple phases
 *
 * (1) Parser    (Source      -> source.Tree)  Load file and parse it into an AST
 *
 * (2) Frontend  (source.Tree -> source.Tree)  Perform name analysis, typechecking, and other
 *                                             rewriting of the source AST
 *
 * (3) Backend   (source.Tree -> core.Tree)    Perform an ANF transformation into core, and
 *                                             other rewritings on the core AST
 *
 * (4) Code Gen  (core.Tree   -> Document)     Generate code in a target language
 *
 */
sealed trait PhaseResult { val source: Source }
case class Parsed(source: Source, tree: ModuleDecl) extends PhaseResult
case class NameResolved(source: Source, tree: ModuleDecl, mod: symbols.Module) extends PhaseResult
// we can notice that nameresolved and typechecked has the same fields. Typer writes to the DB.
// this might change when we switch to elaboration.
case class Typechecked(source: Source, tree: ModuleDecl, mod: symbols.Module) extends PhaseResult
case class CoreTransformed(source: Source, tree: ModuleDecl, mod: symbols.Module, core: effekt.core.ModuleDecl) extends PhaseResult

// TODO MAYBE PASS INPUTS AS PART OF CONTEXT???
// TODO try not to mix Compiler and Context into one object.
// -> but we have a mutual dependency: frontend uses ModuleDB and ModuleDB (in Context) uses frontend

// TODO maybe add the currently processed SOURCE to Context
// like
// // the currently processed module
//  var module: Module = _
//
// we might also be able to drop Module.decl since we now have PhaseResult.
trait Compiler {

  val positions: Positions

  /**
   * Frontend
   */
  private val Frontend = Phase.cached("frontend") {
    Parser andThen
      // performs name analysis and associates Id-trees with symbols
      Namer andThen
      // type checks and annotates trees with inferred types and effects
      Typer andThen
      // uses annotated effects to translate to explicit capability passing
      CapabilityPassing andThen
      // infers regions and prevents escaping of first-class functions
      RegionChecker
  }

  /**
   * Middleend
   */
  private val Middleend = Phase.cached("middleend") {
    Transformer
  }

  /**
   * Backend
   */
  def codeGenerator(implicit C: Context) = C.config.generator() match {
    case "js"           => effekt.generator.JavaScriptMonadic
    case "js-lift"      => effekt.generator.JavaScriptLift
    case "chez-callcc"  => effekt.generator.ChezSchemeCallCC
    case "chez-monadic" => effekt.generator.ChezSchemeMonadic
    case "chez-lift"    => /* LiftInference andThen */ effekt.generator.ChezSchemeLift
  }

  // STUB
  //  object Backend extends Phase[CoreTransformed, Document] {
  //
  //  }

  /**
   * Full compiler pipeline
   */
  val Compiler = Phase.cached("compiler") {
    Frontend andThen Middleend // TODO andThen Backend
  }

  // Compiler Interface
  // ==================
  // As it is used by other parts of the language implementation

  def getAST(source: Source)(implicit C: Context): Option[ModuleDecl] =
    Parser(source).map { res => res.tree }

  def frontend(source: Source)(implicit C: Context): Option[Module] =
    Frontend(source).map { res => res.mod }

  def middleend(source: Source)(implicit C: Context): Option[core.ModuleDecl] =
    Compiler(source).map { res => res.core }

  // TODO Currently the backend is not cached at all
  def generate(source: Source)(implicit C: Context): Option[Document] =
    codeGenerator.apply(source)

  /**
   * Hook potentially used by the generators
   */
  def saveOutput(doc: String, path: String)(implicit C: Context): Unit
}
