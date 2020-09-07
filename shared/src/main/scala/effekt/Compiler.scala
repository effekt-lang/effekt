package effekt

import effekt.context.Context
import effekt.core.{ LiftInference, Transformer }
import effekt.namer.Namer
import effekt.source.ModuleDecl
import effekt.symbols.Module
import effekt.generator.{ ChezSchemeCallCC, ChezSchemeLift, ChezSchemeMonadic, Generator, JavaScript, JavaScriptLift }
import effekt.typer.Typer
import effekt.util.{ SourceTask, VirtualSource }
import org.bitbucket.inkytonik.kiama
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
 */
trait Compiler {

  val positions: Positions

  // Frontend phases
  // ===============

  // The parser needs to be created freshly since otherwise the memo tables will maintain wrong results for
  // new input sources. Even though the contents differ, two sources are considered equal since only the paths are
  // compared.
  def parser = new Parser(positions)
  object namer extends Namer
  object typer extends Typer

  // Backend phases
  // ==============
  object transformer extends Transformer
  object lifter extends LiftInference

  def generatorPhase(implicit C: Context) = C.config.generator() match {
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
      _ <- C.using(module = mod, focus = ast) {
        for {
          _ <- namer(mod)
          _ <- typer(mod)
        } yield ()
      }
    } yield mod
  }

  object lower extends SourceTask[core.ModuleDecl]("lower") {
    def run(source: Source)(implicit C: Context): Option[core.ModuleDecl] = for {
      mod <- frontend(source)
      core <- C.using(module = mod) { transformer(mod) }
    } yield core
  }

  object inferLifts extends SourceTask[core.ModuleDecl]("lifts") {
    def run(source: Source)(implicit C: Context): Option[core.ModuleDecl] = for {
      mod <- frontend(source)
      core <- lower(source)
      lifted <- C.using(module = mod) { lifter(core) }
    } yield lifted
  }

  object generate extends SourceTask[Document]("generate") {
    def run(source: Source)(implicit C: Context): Option[Document] =
      generatorPhase.apply(source)
  }

  /**
   * Hook potentially used by the generators
   */
  def saveOutput(doc: String, path: String)(implicit C: Context): Unit
}
