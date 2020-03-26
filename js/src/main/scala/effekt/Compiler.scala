package effekt

import effekt.context.{ Context, VirtualModuleDB }
import effekt.core.{ JavaScript, Transformer }
import effekt.namer.Namer
import effekt.typer.Typer
import org.bitbucket.inkytonik.kiama.parsing.Success
import org.bitbucket.inkytonik.kiama.util.{ Positions, Source, StringSource }

import scala.scalajs.js.annotation._


@JSExportTopLevel("Compiler")
object Compiler {


  implicit val context = new Context with VirtualModuleDB {
    /**
     * Just runs the frontend (no code generation)
     * When there are errors in processing `source` it returns None
     */
    override def frontend(source: Source): Option[symbols.Module] = ???

    /**
     * Runs both frontend and backend.
     * In case of an error, this variant will abort and report any errors
     */
    override def process(source: Source): symbols.Module = ???
  }

  object parser extends Parser(new Positions)
  object namer extends Namer
  object typer extends Typer
  object transformer extends Transformer
  object codegen extends JavaScript

  @JSExport
  def compile(s: String) = {
    val src = StringSource(s)

    parser.parseAll(parser.program, src) match {
      case Success(result, next) =>
        println("run namer")
        val mod = namer.run(src, result)
        typer.run(result, mod)
        val core = transformer.run(mod)
        codegen.format(core).layout.toString
      case _ => ""
    }
  }
}

object Main extends App {

  val input =
    """module foo
      |
      |effect Print[A](msg: A): Unit
      |
      |def bar() = 42
      |def baz() = do Print("hello world")
      |""".stripMargin

  Compiler.compile(input)
}
