package effekt

import effekt.evaluator.Evaluator
import org.bitbucket.inkytonik.kiama.parsing.ParseResult
import org.bitbucket.inkytonik.kiama.util.{ ParsingREPLWithConfig, Source }
import effekt.source._
import effekt.util.messages.FatalPhaseError
import effekt.symbols.{ BlockSymbol, TypeSymbol, ValueSymbol }
import org.bitbucket.inkytonik.kiama
import kiama.util.Messaging.Messages
import kiama.util.Console

// for now we only take expressions
trait EffektRepl extends ParsingREPLWithConfig[Tree, EffektConfig] {

  object driver extends Driver
  object evaluator extends Evaluator

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

  def reset() = {
    definitions = Nil
    imports = Nil
  }


  /**
   * Adapted from the kiama/lambda2/Lambda example
   *
   * Process a user input line by intercepting meta-level commands to
   * update the evaluation mechanisms.  By default we just parse what
   * they type into an expression.
   */
  override def processline(source: Source, console: Console, config: EffektConfig): Option[EffektConfig] = {

    // Shorthand access to the output emitter
    val output = config.output()

    /*
     * Print help about the available commands.
     */
    def printHelp() : Unit = {
        output.emitln(
          """|<expression>         print the result of evaluating exp
             |<definition>         add a definition to the REPL context
             |import <path>        add an import to the REPL context
             |
             |:imports             list all current imports
             |:reset               reset the REPL state
             |:help                print this help message
             |:quit                quit this REPL""".stripMargin)
    }

    source.content match {

      case Command(List(":reset")) =>
        reset()
        Some(config)

      case Command(List(":imports")) =>
        output.emitln(imports.map { i => i.path }.mkString("\n"))
        Some(config)

      case Command(List(":help")) | Command(List(":h")) =>
        printHelp()
        Some(config)

      case Command(List(":quit")) | Command(List(":q")) =>
        None

      // Otherwise it's an expression for evaluation
      case _ =>
        super.processline(source, console, config)
    }
  }


  /**
   * Processes the given tree, but only commits changes to definitions and
   * imports, if they typecheck.
   */
  def process(source: Source, tree: Tree, config: EffektConfig): Unit = tree match {
    case e: Expr =>
      val decl = makeModule(e, imports, definitions)

      reportOrElse(source, frontend(source, decl, config)) { cu =>
        val value = evaluator.run(cu, driver.context)

        val mainSym = cu.exports.terms("main")
        val mainTpe = driver.context.blockType(mainSym)
        val out = s"${value} : ${mainTpe.ret}"

        config.output().emitln(out)
      }

    case i: Import if imports.forall { other => other.path != i.path } =>
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

        // try to find the symbol for the def to print the type
        driver.context.get(d) match {
          case v: ValueSymbol => Some(driver.context.valueType(v))
          case b: BlockSymbol => Some(driver.context.blockType(b))
          case t => None
        } map { tpe =>
          config.output().emitln(tpe)
        }
      }

    case _ => ()
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


  object Command {
    def unapply(str: String): Option[List[String]] =
      if (str.startsWith(":")) Some(str.split(' ').toList) else None

  }
}
object Repl extends EffektRepl