package effekt

import effekt.context.Context
import effekt.core.{ LiftInference, Transformer }
import effekt.namer.Namer
import effekt.regions.RegionChecker
import effekt.source.{ CapabilityPassing, ModuleDecl }
import effekt.symbols.Module
import effekt.typer.Typer
import effekt.util.{ SourceTask, VirtualSource }

import kiama.output.PrettyPrinterTypes.Document
import kiama.util.{ Positions, Source }

/**
 * "Pure" compiler without reading or writing to files
 *
 * All methods return Option, the errors are reported in the given context
 *
 * We distinguish between Phases and Tasks. Phases, by default, perform no memoization
 * while task use the "build system" abstraction in `Task`, track dependencies and
 * avoid rebuilding by memoization.
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
trait Compiler {

  val positions: Positions

  /**
   * (1) Parser
   *
   * Note: The parser needs to be created freshly since otherwise the memo tables will maintain wrong results for
   * new input sources. Even though the contents differ, two sources are considered equal since only the paths are
   * compared.
   */
  def parser = new Parser(positions)

  /**
   * (2) Frontend
   */
  val frontendPhases: List[Phase[ModuleDecl, ModuleDecl]] = List(
    // performs name analysis and associates Id-trees with symbols
    new Namer,
    // type checks and annotates trees with inferred types and effects
    new Typer,
    // uses annotated effects to translate to explicit capability passing
    new CapabilityPassing,
    // infers regions and prevents escaping of first-class functions
    new RegionChecker
  )

  /**
   * (3) Backend
   */
  object transformer extends Transformer
  val backendPhases: List[Phase[core.ModuleDecl, core.ModuleDecl]] = List(
    // optional phase, only run for `Config.requiresLift`
    new LiftInference
  )

  /**
   * (4) Code Generation
   */
  def codeGenerator(implicit C: Context) = C.config.generator() match {
    case "js"           => new effekt.generator.JavaScript
    case "js-lift"      => new effekt.generator.JavaScriptLift
    case "chez-callcc"  => new effekt.generator.ChezSchemeCallCC
    case "chez-monadic" => new effekt.generator.ChezSchemeMonadic
    case "chez-lift"    => new effekt.generator.ChezSchemeLift
  }

  // Tasks
  // =====

  object getAST extends SourceTask[ModuleDecl]("ast") {
    def run(source: Source)(implicit C: Context): Option[ModuleDecl] = source match {
      case VirtualSource(decl, _) => Some(decl)
      case _ => parser(source)
    }
  }

  object frontend extends SourceTask[Module]("frontend") {
    def run(source: Source)(implicit C: Context): Option[Module] = for {
      ast <- getAST(source)
      mod = Module(ast, source)
      transformedAst <- C.using(module = mod, focus = ast) {
        Phase.run(ast, frontendPhases)
      }
    } yield mod.setAst(transformedAst)
  }

  object backend extends SourceTask[core.ModuleDecl]("backend") {
    def run(source: Source)(implicit C: Context): Option[core.ModuleDecl] = for {
      mod <- frontend(source)
      core <- C.using(module = mod) { transformer(mod) }
      transformed <- C.using(module = mod) {
        Phase.run(core, backendPhases)
      }
    } yield transformed
  }

  object generate extends SourceTask[Document]("generate") {
    def run(source: Source)(implicit C: Context): Option[Document] =
      codeGenerator.apply(source)
  }

  /**
   * Hook potentially used by the generators
   */
  def saveOutput(doc: String, path: String)(implicit C: Context): Unit
}
