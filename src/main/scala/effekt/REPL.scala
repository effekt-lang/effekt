package effekt

import org.bitbucket.inkytonik.kiama.parsing.ParseResult
import org.bitbucket.inkytonik.kiama.util.{ ParsingREPLWithConfig, Source }
import effekt.source.{ Call, Def, Expr, FunDef, IdDef, IdRef, ModuleDecl, Return, Tree, UnitLit, ValueArgs }
import effekt.util.messages.FatalPhaseError

// for now we only take expressions
trait EffektRepl extends ParsingREPLWithConfig[Tree, EffektConfig] {

  object driver extends Driver

  var definitions: List[Def] = Nil

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
    parsers.parseAll(parsers.definitionOrExpression, source)
  }

  def process(source: Source, defOrExpr: Tree, config: EffektConfig): Unit =
    try {
      defOrExpr match {
        case e: Expr => runExpr(source, Call(IdRef("println"), Nil, List(ValueArgs(List(e)))), config)
        case d: Def =>
          definitions = definitions :+ d
          driver.process(source, makeModule(), config)
      }
    } catch {
      case FatalPhaseError(msg, reporter) =>
        val ctx = driver.context
        ctx.error(msg)
        driver.report(source, ctx.buffer.get, config)
    }

  def runExpr(source: Source, expr: Expr, config: EffektConfig): Unit =
    driver.process(source, makeModule(expr), config)

  private def makeModule(expr: Expr = UnitLit()): ModuleDecl =
    ModuleDecl("lib/interactive", Nil,
      definitions :+ FunDef(IdDef("main"), Nil, Nil, None,
        Return(expr)))
}
object Repl extends EffektRepl