package effekt

import effekt.context.{ Context, VirtualModuleDB }
import effekt.evaluator.Evaluator
import effekt.util.messages.FatalPhaseError
import org.bitbucket.inkytonik.kiama.output.PrettyPrinterTypes.Document
import org.bitbucket.inkytonik.kiama.util.{ Position, Positions, Source, StringSource }

import scala.scalajs.js.annotation._

class CompilerJS extends Compiler {

  val positions: Positions = new Positions

  implicit object context extends Context(this) with VirtualModuleDB {
    // TODO call setup instead of manually setting up everything here
    config = new EffektConfig {}
  }

  var output: StringBuilder = _

  def compile(s: String): String = {
    output = new StringBuilder
    context.buffer.clear()
    compile(StringSource(s)) match {
      case None =>
        sys error "Error"
      case Some(_) =>
        output.toString
    }
  }

  object evaluator extends Evaluator

  def eval(s: String): Unit = {
    output = new StringBuilder
    context.buffer.clear()
    compile(StringSource(s)) match {
      case None =>
        sys error "Error"
      case Some(mod) =>
        val result = evaluator.run(mod)
        println(result)
    }
  }

  /**
   * Don't output module declarations
   */
  override def backend(source: Source, mod: core.ModuleDecl)(implicit C: Context): Option[Document] = try {
    Some(codegen.pretty(codegen.global(mod)))
  } catch {
    case FatalPhaseError(msg, reporter) =>
      reporter.error(msg)
      None
  }

  override def saveOutput(js: Document, unit: symbols.Module)(implicit C: Context): Unit = {
    output.append(s"\n// Result of compiling module: ${unit.name}\n")
    output.append(js.layout)
    output.append("\n\n")
  }
}

class CodeInfoJS(input: String, val positions: Positions) extends Compiler with Intelligence {

  implicit object context extends Context(this) with VirtualModuleDB {
    config = new EffektConfig {}
  }

  val source = StringSource(input)

  val mod = frontend(source) getOrElse {
    sys error context.buffer.get.mkString("\n\n")
  }
  context.setup(mod.decl, context.config)

  @JSExport
  def definitionAt(line: Int, col: Int): Option[effekt.source.Tree] = {
    val p = Position(line, col, source)
    getDefinitionAt(p)(context)
  }

  @JSExport
  def infoAt(line: Int, col: Int): Option[String] = {
    val p = Position(line, col, source)
    getSymbolAt(p) flatMap { case (tree, sym) => getInfoOf(sym).map { _.fullDescription } }
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
  def IDE(s: String): CodeInfoJS = new CodeInfoJS(s, new Positions)
}

object Main extends App {

  val input =
    """module foo
      |
      |effect Print[A](msg: A): Unit
      |
      |def bar() = "hello world"
      |def baz() = try {
      |  do Print(bar())
      |} with {
      |  case Print(s) => println(s)
      |}
      |""".stripMargin

  println(CompilerJS.compile(input))

  //  val ide = CompilerJS.IDE(input)
  //  println(ide.infoAt(9, 11))
}
