package effekt

import effekt.context.Context
import effekt.core.{ JavaScript, Transformer }
import effekt.namer.Namer
import effekt.source.ModuleDecl
import effekt.symbols.Module
import effekt.typer.Typer
import effekt.util.Task
import effekt.util.messages.FatalPhaseError
import org.bitbucket.inkytonik.kiama
import kiama.output.PrettyPrinterTypes.Document
import kiama.parsing.{ NoSuccess, Success }
import kiama.util.{ Positions, Source }

import scala.collection.mutable

/**
 * "Pure" compiler without reading or writing to files
 *
 * All methods return Option, the errors are reported in the given context
 */
trait Compiler {

  val positions: Positions

  // Frontend phases
  // ===============
  lazy val parser = new Parser(positions)
  lazy val namer = new Namer()
  lazy val typer = new Typer
  lazy val frontend = (namer andThen typer)

  // Backend phases
  // ==============
  lazy val transformer = new Transformer
  lazy val codegen = new JavaScript

  /**
   * The full compiler pipeline from source to output
   */
  def compile(source: Source)(implicit C: Context): Option[Module] =
    parsing(source) { pipeline }

  /**
   * Variant: Compiler without parser (used by Repl, since Kiama does the parsing)
   */
  def compile(ast: ModuleDecl, source: Source)(implicit C: Context): Option[Module] =
    withModule(ast, source) { pipeline }

  /**
   * The full pipeline after parsing
   */
  private def pipeline(mod: Module)(implicit C: Context): Option[Module] = for {
    mod <- frontend(mod)
    js <- backend(mod)
    _ = saveOutput(js, mod)
  } yield mod

  /**
   * Frontend: Parser -> Namer -> Typer
   */
  def frontend(source: Source)(implicit C: Context): Option[Module] =
    parsing(source) { mod => frontend(mod) }

  /**
   * Variant: Frontend without parser (used by Repl)
   */
  def frontend(ast: ModuleDecl, source: Source)(implicit C: Context): Option[Module] =
    withModule(ast, source) { mod => frontend(mod) }

  /**
   * Backend: Module -> Document
   */
  def backend(mod: Module)(implicit C: Context): Option[Document] =
    (transformer andThen codegen)(mod)

  /**
   * Output writer: Document -> IO
   */
  def saveOutput(js: Document, unit: Module)(implicit C: Context): Unit

  // Utils
  // =====

  /**
   * Parsing source and brings the module into scope
   */
  private def parsing[R](source: Source)(f: Module => Option[R])(implicit C: Context): Option[R] =
    parser(source).flatMap { ast => withModule(ast, source) { f } }

  /**
   * Don't create a fresh module every time, but reuse the existing one. Modules are symbols, they are compared
   * by identity. Otherwise importing the same module in two files will lead to different types with the same names.
   */
  private val moduleCache = mutable.HashMap.empty[Source, Module]
  private def withModule[R](ast: ModuleDecl, source: Source)(f: Module => Option[R])(implicit C: Context): Option[R] = {
    val mod = moduleCache.getOrElseUpdate(source, Module(ast, source))
    C in {
      C.module = mod
      C.focus = ast
      f(mod)
    }
  }

}
