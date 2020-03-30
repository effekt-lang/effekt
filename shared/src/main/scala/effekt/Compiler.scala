package effekt

import effekt.context.Context
import effekt.core.{ JavaScript, Transformer }
import effekt.namer.Namer
import effekt.source.ModuleDecl
import effekt.symbols.Module
import effekt.typer.Typer
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

  def saveOutput(js: Document, unit: Module)(implicit C: Context): Unit

  // Frontend phases
  // ===============
  lazy val parser = new Parser(positions).cached
  lazy val namer = new Namer()
  lazy val typer = new Typer
  lazy val frontend = (namer andThen typer).cached

  // Backend phases
  // ==============
  lazy val transformer = new Transformer
  lazy val codegen = new JavaScript

  /**
   * The full compiler pipeline from source to output
   */
  def compile(source: Source)(implicit C: Context): Option[Module] =
    for {
      ast <- parser(source)
      mod <- frontend(source, ast)
      js <- backend(mod)
      _ = saveOutput(js, mod)
    } yield mod

  /**
   * Frontend: Parser -> Namer -> Typer
   */
  def frontend(source: Source)(implicit C: Context): Option[Module] =
    parser(source) flatMap { mod => frontend(source, mod) }

  /**
   * Backend: Module -> Document
   */
  def backend(mod: Module)(implicit C: Context): Option[Document] =
    (transformer andThen codegen)(mod)

}
