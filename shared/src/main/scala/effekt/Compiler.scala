package effekt

import effekt.context.Context
import effekt.core.{ ChezScheme, JavaScript, Transformer }
import effekt.namer.Namer
import effekt.source.ModuleDecl
import effekt.symbols.Module
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

  // This is overwritten in the JavaScript version of the compiler (to switch to another JS module system)
  // TODO group code generation, naming conventions, and writing to files into one abstraction to be able to
  //      easily switch
  def generator(implicit C: Context): Phase[effekt.core.ModuleDecl, Document] = C.config.generator() match {
    case "js" => new JavaScript()
    case "cs" => new ChezScheme()
    case _    => sys error "Generator missing"
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

  object computeCore extends SourceTask[core.ModuleDecl]("core") {
    def run(source: Source)(implicit C: Context): Option[core.ModuleDecl] = for {
      mod <- frontend(source)
      core <- C.using(module = mod) { transformer(mod) }
    } yield core
  }

  object generate extends SourceTask[Document]("generate") {
    def run(source: Source)(implicit C: Context): Option[Document] = for {
      mod <- frontend(source)
      _ = println(mod.dependencies.map { m => "module " + m.path }) // DEBUG
      core <- computeCore(source)
      doc <- C.using(module = mod) { generator.apply(core) }
    } yield doc
  }

  // TODO change result from Unit to File
  object compile extends SourceTask[Unit]("compile") {
    def run(source: Source)(implicit C: Context): Option[Unit] = for {
      mod <- frontend(source)
      target <- generate(source)
      _ = saveOutput(target, mod)
    } yield ()
  }

  /**
   * Output writer: Document -> IO
   *
   * TODO convert into task?
   */
  def saveOutput(js: Document, unit: Module)(implicit C: Context): Unit

}
