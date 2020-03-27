package effekt

import effekt.context.{ Context, VirtualModuleDB }
import org.bitbucket.inkytonik.kiama.output.PrettyPrinterTypes.Document
import org.bitbucket.inkytonik.kiama.util.{ Positions, StringSource }

import scala.scalajs.js.annotation._


class CompilerJS extends Compiler {

  val positions: Positions = new Positions

  implicit object context extends Context(this) with VirtualModuleDB

  var output: StringBuilder = _

  def compile(s: String): String = {
    output = new StringBuilder
    context.buffer.clear()
    compile(StringSource(s)) match {
      case None =>
        println(context.buffer.get.mkString("\n\n"))
        sys error "Error"
      case Some(_) =>
        println(context.buffer.get.mkString("\n\n"))
        output.toString
    }
  }

  override def saveOutput(js: Document, unit: symbols.Module)(implicit C: Context): Unit = {
    output.append(s"\n// Result of compiling module: ${unit.name}\n")
    output.append(js.layout)
    output.append("\n\n")
  }
}

@JSExportTopLevel("compiler")
object CompilerJS {

  @JSExport
  def compile(s: String): String =
    new CompilerJS().compile(s)
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
}
