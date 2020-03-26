package effekt

import effekt.context.{ Context, VirtualModuleDB }
import org.bitbucket.inkytonik.kiama.output.PrettyPrinterTypes.Document
import org.bitbucket.inkytonik.kiama.util.{ Positions, Source, StringSource }

import scala.scalajs.js.annotation._


@JSExportTopLevel("compiler")
object CompilerJS extends Compiler {

  val positions: Positions = new Positions

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


  @JSExport
  def compile(s: String) = (for {
    src <- Some(StringSource(s))
    ast <- parse(src)
    mod <- frontend(src, ast)
    cor <- middleend(src, mod)
    js  <- backend(src, cor)
  } yield js).getOrElse("")

  override def saveOutput(js: Document, unit: symbols.Module)(implicit C: Context): Unit = ???
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

  println(CompilerJS.compile(input))
}
