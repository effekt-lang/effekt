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

    for {
      mod <- frontend(src)
      _ <- compile(src)
      program = output.toString
      command = s"""(function() {
        | var module = {}
        |$program
        |
        |return ${mod.name}.main().run();
        |})()
        |""".stripMargin
    } yield scalajs.js.eval(command)
  }.orNull

  override def saveOutput(js: Document, unit: symbols.Module)(implicit C: Context): Unit = {
    output.append(s"\n// Result of compiling module: ${unit.name}\n")
    output.append(js.layout)
    output.append("\n\n")
  }
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
