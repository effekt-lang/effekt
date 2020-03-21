package effekt

import org.bitbucket.inkytonik.kiama.parsing.ParseResult
import org.bitbucket.inkytonik.kiama.util.{ ParsingREPLWithConfig, Source }
import effekt.source._
import effekt.util.messages.FatalPhaseError

import org.bitbucket.inkytonik.kiama
import kiama.util.Messaging.Messages

// for now we only take expressions
trait EffektRepl extends ParsingREPLWithConfig[Tree, EffektConfig] {

  object driver extends Driver

  var definitions: List[Def] = Nil
  var imports: List[Import] = Nil

  val banner =
    """|  _____     ______  __  __     _    _
       | (_____)   |  ____|/ _|/ _|   | |  | |
       |   ___     | |__  | |_| |_ ___| | _| |_
       |  (___)    |  __| |  _|  _/ _ \ |/ / __|
       |  _____    | |____| | | ||  __/   <| |_
       | (_____)   |______|_| |_| \___|_|\_\\__|
       |
       | Welcome to the Effekt interpreter. Enter a top-level definition, or
       | an expression to evaluate.
       |""".stripMargin

  def createConfig(args: Seq[String]) = driver.createConfig(args)

  def parse(source: Source): ParseResult[Tree] = {
    val parsers = new Parser(positions)
    parsers.parseAll(parsers.repl, source)
  }

  /**
   * Processes the given tree, but only commits changes to definitions and
   * imports, if they typecheck.
   */
  def process(source: Source, tree: Tree, config: EffektConfig): Unit = tree match {
    case e: Expr =>
      val decl = makeModule(
        Call(IdRef("println"), Nil, List(ValueArgs(List(e)))),
        imports,
        definitions)

      driver.process(source, decl, config)

    // TODO see whether this path has already been imported
    case i: Import =>
      val extendedImports = imports :+ i
      val decl = makeModule(UnitLit(), extendedImports, definitions)

      reportOrElse(source, frontend(source, decl, config)) { cu =>
        imports = extendedImports
      }

    case d: Def =>
      val extendedDefs = definitions :+ d
      val decl = makeModule(UnitLit(), imports, extendedDefs)
      reportOrElse(source, frontend(source, decl, config)) { cu =>
        definitions = extendedDefs
      }
  }


  /**
   * Only executes the block f, if there are no errors
   *
   * TODO move into driver
   */
  def reportOrElse(source: Source, res: Either[Messages, CompilationUnit])(f: CompilationUnit => Unit): Unit = res match {
    case Right(cu) =>
      val buffer = driver.context.buffer
      if (buffer.hasErrors) {
        driver.report(source, buffer.get, driver.context.config)
      } else {
        f(cu)
      }
    case Left(msgs) =>
      driver.report(source, msgs, driver.context.config)
  }

  def frontend(source: Source, ast: ModuleDecl, config: EffektConfig): Either[Messages, CompilationUnit] = {
    driver.context.setup(ast, config)
    driver.frontend(source, ast, driver.context)
  }

  private def makeModule(expr: Expr, imports: List[Import], definitions: List[Def]): ModuleDecl =
    ModuleDecl("lib/interactive", imports,
      definitions :+ FunDef(IdDef("main"), Nil, Nil, None,
        Return(expr)))
}
object Repl extends EffektRepl