package effekt

import effekt.context.Context
import effekt.core.{ JavaScript, Transformer }
import effekt.namer.Namer
import effekt.typer.Typer
import org.bitbucket.inkytonik.kiama.parsing.Success
import org.bitbucket.inkytonik.kiama.util.{ Positions, Source, StringSource }

object Main extends App {

  object parsers extends Parser(new Positions)

  val input =
    """module foo
      |
      |effect Print[A](msg: A): Unit
      |
      |def bar() = 42
      |def baz() = do Print("hello world")
      |""".stripMargin

  implicit val context = new Context {
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

  object namer extends Namer
  object typer extends Typer
  object transformer extends Transformer
  object codegen extends JavaScript

  val src = StringSource(input)

  parsers.parseAll(parsers.program, src) match {
    case Success(result, next) =>
      println("run namer")
      val mod = namer.run(src, result)
      typer.run(result, mod)
      val core = transformer.run(mod)
      println(codegen.format(core).layout)
    case _ => ()
  }
}