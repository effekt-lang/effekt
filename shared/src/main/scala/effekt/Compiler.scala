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

/**
 * "Pure" compiler without reading or writing to files
 *
 * All methods return Option, the errors are reported in the given context
 */
trait Compiler {

  def positions: Positions

  def saveOutput(js: Document, unit: Module)(implicit C: Context): Unit


  // Frontend phases
  // ===============
  object parser extends Parser(positions)
  object namer extends Namer
  object typer extends Typer

  // Middleend phases
  // ================
  object transformer extends Transformer
  object prettyCore extends core.PrettyPrinter

  // Backend phases
  // ==============
  object js extends JavaScript


  /**
   * The full compiler pipeline from source to output
   */
  def compile(source: Source)(implicit C: Context): Option[Module] =
    parse(source) flatMap { ast => pipeline(source, ast) }


  /**
   * The pipeline from ast to output
   */
  def pipeline(source: Source, ast: ModuleDecl)(implicit C: Context): Option[Module] = for {
    mod  <- frontend(source, ast)
    core <- middleend(source, mod)
    js   <- backend(source, core)
    _    = saveOutput(js, mod)
  } yield mod


  /**
   * Parser: Source -> AST
   *
   * adapted from: kiama.util.compiler.makeast
   */
  def parse(source: Source)(implicit C: Context): Option[ModuleDecl] =
    parser.parseAll(parser.program, source) match {
      case Success(ast, _) =>
        Some(ast)

      case res : NoSuccess =>
        val input = res.next
        positions.setStart(res, input.position)
        positions.setFinish(res, input.nextPosition)
        C.error(res.message)
        None
    }



  /**
   * Frontend: Parser -> Namer -> Typer
   */
  def frontend(source: Source)(implicit C: Context): Option[Module] =
    parse(source) flatMap { mod => frontend(source, mod) }


  /**
   * Frontend: Namer -> Typer
   */
  def frontend(source: Source, ast: ModuleDecl)(implicit C: Context): Option[Module] = try {
    val mod = namer.run(source, ast)
    typer.run(ast, mod)

    if (C.buffer.hasErrors) {
      None
    } else {
      Some(mod)
    }
  } catch {
    case FatalPhaseError(msg, reporter) =>
      reporter.error(msg)
      None
  }


  /**
   * Middleend: Effekt -> Core
   */
  def middleend(source: Source, unit: Module)(implicit C: Context): Option[core.ModuleDecl] = try {
    Some(transformer.run(unit))
  } catch {
    case FatalPhaseError(msg, reporter) =>
      reporter.error(msg)
      None
  }


  /**
   * Backend: Effekt -> Core -> JavaScript
   */
  def backend(source: Source, mod: core.ModuleDecl)(implicit C: Context): Option[Document] = try {
    Some(js.format(mod))
  } catch {
    case FatalPhaseError(msg, reporter) =>
      reporter.error(msg)
      None
  }
}