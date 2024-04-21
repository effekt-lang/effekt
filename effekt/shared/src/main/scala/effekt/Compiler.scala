package effekt

import effekt.context.Context
import effekt.namer.Namer
import effekt.source.{ AnnotateCaptures, ExplicitCapabilities, ModuleDecl }
import effekt.symbols.Module
import effekt.typer.{ Typer }
import effekt.util.messages.FatalPhaseError
import effekt.util.{ SourceTask, Task, VirtualSource, paths }
import kiama.output.PrettyPrinterTypes.Document
import kiama.util.{ Positions, Source }


enum PhaseResult {

  val source: Source

  case Parsed(source: Source, tree: ModuleDecl)
  case NameResolved(source: Source, tree: ModuleDecl, mod: symbols.Module)
  case Typechecked(source: Source, tree: ModuleDecl, mod: symbols.Module)

}
export PhaseResult.*

enum Stage { case Core; case Target; }

trait Compiler[Executable] {


  def extension: String

  /**
   * Used by LSP server (Intelligence) to map positions to source trees
   */
  def getAST(source: Source)(using Context): Option[ModuleDecl] = None
  def runFrontend(source: Source)(using Context): Option[Module] = None

  def validate(source: Source, mod: Module)(using Context): Unit = ()

  def prettyIR(source: Source, stage: Stage)(using Context): Option[Document]

  def treeIR(source: Source, stage: Stage)(using Context): Option[Any]

  def compile(source: Source)(using Context): Option[(Map[String, String], Executable)]

  val CachedParser: Phase[Source, Parsed] = ???

  val Frontend: Phase[Source, Typechecked] = ???

  def path(m: symbols.Module)(using C: Context): String = ""
}
