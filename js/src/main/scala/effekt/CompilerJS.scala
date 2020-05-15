package effekt

import effekt.context.{ Context, VirtualModuleDB }
import effekt.core.JavaScriptGlobal
import effekt.symbols.Module
import effekt.util.messages.FatalPhaseError
import org.bitbucket.inkytonik.kiama.output.PrettyPrinterTypes.Document
import org.bitbucket.inkytonik.kiama.util.{ Message, Messaging, Position, Positions, Severities, Source, StringSource }

import scala.scalajs.js
import js.JSConverters._
import scala.scalajs.js.annotation._

object JSConfig extends EffektConfig {}

class CompilerJS extends Compiler {

  val positions: Positions = new Positions

  implicit object context extends Context(this) with VirtualModuleDB

  /**
   * Don't output amdefine module declarations
   */
  override lazy val generator = new JavaScriptGlobal

  var output: StringBuilder = _

  def compile(s: String): String = {
    output = new StringBuilder
    context.setup(JSConfig)
    compile(StringSource(s)) match {
      case None =>
        println(context.buffer.get)
        sys error "Could not compile with EffektJS"
      case Some(_) =>
        output.toString
    }
  }

  def eval(s: String): Unit = {
    output = new StringBuilder
    val src = StringSource(s)
    context.setup(JSConfig)

    for (mod <- frontend(src); _ <- compile(src)) {
      val program = output.toString
      val command =
        s"""(function() {
            | var module = {}
            |$program
            |
            |return ${mod.name}.main().run();
            |})()
            |""".stripMargin
      scalajs.js.eval(command)
    }
  }

  override def saveOutput(js: Document, unit: symbols.Module)(implicit C: Context): Unit = {
    output.append(s"\n// Result of compiling module: ${unit.name}\n")
    output.append(js.layout)
    output.append("\n\n")
  }
}

// the LSP types
// https://github.com/microsoft/vscode-languageserver-node/blob/master/types/src/main.ts
object lsp {
  // LSP positions are zero indexed
  class Position(val line: Int, val character: Int) extends js.Object
  class Range(val from: Position, val to: Position) extends js.Object
  class Diagnostic(val range: Range, val severity: DiagnosticSeverity, val message: String) extends js.Object {
    val source = "effekt"
  }
  type DiagnosticSeverity = Int
  object DiagnosticSeverity {
    val Error = 1
    val Warning = 2
    val Information = 3
    val Hint = 4
  }
}

class LanguageServer extends Compiler with Intelligence {

  implicit object context extends Context(this) with VirtualModuleDB

  val positions: Positions = new Positions
  object messaging extends Messaging(positions)

  context.setup(JSConfig)

  var source = StringSource("")

  @JSExport
  def definitionAt(line: Int, col: Int): Option[effekt.source.Tree] = {
    val p = Position(line, col, source)
    getDefinitionAt(p)(context)
  }

  @JSExport
  def infoAt(line: Int, col: Int): Option[String] = {
    val p = Position(line, col, source)
    for {
      (tree, sym) <- getSymbolAt(p)
      info <- getInfoOf(sym)
    } yield info.fullDescription
  }

  @JSExport
  def typecheck(): js.Array[lsp.Diagnostic] = {
    context.buffer.clear()
    frontend(source)
    context.buffer.get.map(messageToDiagnostic).toJSArray
  }

  private def messageToDiagnostic(m: Message) = {
    val from = messaging.start(m).map(convertPosition).getOrElse(null)
    val to = messaging.finish(m).map(convertPosition).getOrElse(null)
    new lsp.Diagnostic(new lsp.Range(from, to), convertSeverity(m.severity), m.label)
  }

  private def convertPosition(p: Position): lsp.Position =
    new lsp.Position(p.line - 1, p.column - 1)

  private def convertSeverity(s: Severities.Severity): lsp.DiagnosticSeverity = s match {
    case Severities.Error       => lsp.DiagnosticSeverity.Error
    case Severities.Warning     => lsp.DiagnosticSeverity.Warning
    case Severities.Information => lsp.DiagnosticSeverity.Information
    case Severities.Hint        => lsp.DiagnosticSeverity.Hint
  }

  @JSExport
  def updateContents(input: String) = {
    source = StringSource(input)
  }

  override def saveOutput(js: Document, unit: symbols.Module)(implicit C: Context): Unit = ()
}

@JSExportTopLevel("effekt")
object CompilerJS {

  @JSExport
  def compile(s: String): String =
    new CompilerJS().compile(s)

  @JSExport
  def eval(s: String): Unit =
    new CompilerJS().eval(s)

  @JSExport
  def LanguageServer(): LanguageServer = new LanguageServer()
}
