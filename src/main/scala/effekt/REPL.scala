package effekt

import effekt.evaluator.Evaluator
import org.bitbucket.inkytonik.kiama.parsing.{ NoSuccess, ParseResult, Success }
import effekt.source._
import effekt.util.messages.FatalPhaseError
import effekt.symbols.{ BlockSymbol, DeclPrinter, TypeSymbol, ValueSymbol }
import org.bitbucket.inkytonik.kiama
import kiama.util.Messaging.{ Messages, message }
import kiama.util.{ Console, ParsingREPLWithConfig, Source, StringSource }

// for now we only take expressions
class EffektRepl(driver: Driver) extends ParsingREPLWithConfig[Tree, EffektConfig] {

  object evaluator extends Evaluator

  var module: ReplModule = emptyModule

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
       |
       | To print the available commands, enter :help
       |""".stripMargin

  def createConfig(args: Seq[String]) = driver.createConfig(args)

  // Adapting Kiama REPL's driver to work with an already processed config
  def run(config: EffektConfig): Unit = {
    val out = config.output()
    out.emitln(banner)

    if (config.time())
      profiler.time(processlines(config))
    else
      processlines(config)

    config.output().emitln
  }

  def parse(source: Source): ParseResult[Tree] = {
    val parsers = new Parser(positions)
    parsers.parseAll(parsers.repl, source)
  }

  def reset() = { module = emptyModule }


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
          """|<expression>              print the result of evaluating exp
             |<definition>              add a definition to the REPL context
             |import <path>             add an import to the REPL context
             |
             |:status                   show the current REPL environment
             |:type (:t) <expression>   show the type of an expression
             |:imports                  list all current imports
             |:reset                    reset the REPL state
             |:help (:h)                print this help message
             |:quit (:q)                quit this REPL""".stripMargin)
    }

    source.content match {

      case Command(List(":status")) =>
        status(config)
        Some(config)

      case Command(List(":reset")) =>
        reset()
        Some(config)

      case Command(List(":l", path)) =>
        val src = StringSource(s"import $path", source.name)
        super.processline(src, console, config)
        Some(config)

      case Command(List(":imports")) =>
        output.emitln(module.imports.map { i => i.path }.mkString("\n"))
        Some(config)

      case Command(List(":help")) | Command(List(":h")) =>
        printHelp()
        Some(config)

      case Command(List(":quit")) | Command(List(":q")) =>
        None

      case Command(":type" :: _) =>
        val exprSource = StringSource(source.content.stripPrefix(":type "), source.name)
        typecheck(exprSource, config)
        Some(config)

      case Command(":t" :: _) =>
        val exprSource = StringSource(source.content.stripPrefix(":t "), source.name)
        typecheck(exprSource, config)
        Some(config)

      // Otherwise it's an expression for evaluation
      case _ =>
        super.processline(source, console, config)
    }
  }


  def status(config: EffektConfig): Unit = {
    import symbols._

    val source = StringSource("")
    val out = config.output()

    module.imports.foreach { im =>
      out.emitln(s"import ${im.path}")
    }
    out.emitln("")

    reportOrElse(source, frontend(source, module.make(UnitLit()), config)) { cu =>
      val ctx = driver.context

      module.definitions.foreach {
        case u: Def =>
          val sym = ctx.get(u)
          out.emitln(DeclPrinter(sym, driver.context))
      }
    }
  }


  // TODO refactor and clean this up
  def typecheck(source: Source, config: EffektConfig): Unit =
    parse(source) match {
      case Success(e: Expr, _) =>
        reportOrElse(source, frontend(source, module.make(e), config)) { cu =>
          val mainSym = cu.exports.terms("main")
          val mainTpe = driver.context.blockType(mainSym)
          config.output().emitln(mainTpe.ret)
        }

      case Success(other, _) =>
        config.output().emitln("Can only show type of expressions")

      // this is usually encapsulated in REPL.processline
      case res : NoSuccess =>
        val pos = res.next.position
        positions.setStart(res, pos)
        positions.setFinish(res, pos)
        val messages = message(res, res.message)
        report(source, messages, config)
    }


  /**
   * Processes the given tree, but only commits changes to definitions and
   * imports, if they typecheck.
   */
  def process(source: Source, tree: Tree, config: EffektConfig): Unit = tree match {
    case e: Expr =>

      reportOrElse(source, frontend(source, module.make(e), config)) { cu =>
        val value = evaluator.run(cu, driver.context)
        config.output().emitln(value)
      }

    case i: Import if !module.contains(i) =>
      val extendedImports = module + i
      val decl = extendedImports.make(UnitLit())

      reportOrElse(source, frontend(source, decl, config)) { cu =>
        module = extendedImports
      }

    case d: Def =>
      val extendedDefs = module + d
      val decl = extendedDefs.make(UnitLit())
      reportOrElse(source, frontend(source, decl, config)) { cu =>
        module = extendedDefs

        // try to find the symbol for the def to print the type
        driver.context.get(d) match {
          case v: ValueSymbol =>
            Some(driver.context.valueType(v))
          case b: BlockSymbol =>
            Some(driver.context.blockType(b))
          case t =>
            None
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
        report(source, buffer.get, driver.context.config)
      } else {
        f(cu)
      }
    case Left(msgs) =>
      report(source, msgs, driver.context.config)
  }

  def frontend(source: Source, ast: ModuleDecl, config: EffektConfig): Either[Messages, CompilationUnit] = {
    driver.context.setup(ast, config)
    driver.frontend(source, ast, driver.context)
  }

  object Command {
    def unapply(str: String): Option[List[String]] =
      if (str.startsWith(":")) Some(str.split(' ').toList) else None

  }

  /**
   * A virtual module to which we add by using the REPL
   */
  case class ReplModule(
    definitions: List[Def],
    imports: List[Import]
  ) {
    def +(d: Def) = copy(definitions = definitions :+ d)
    def +(i: Import) = copy(imports = imports :+ i)

    def contains(im: Import) = imports.exists { other => im.path == other.path }

    /**
     * Create a module declaration using the given expression as body of main
     */
    def make(expr: Expr): ModuleDecl =
      ModuleDecl("lib/interactive", imports,
        definitions :+ FunDef(IdDef("main"), Nil, List(ValueParams(Nil)), None,
          Return(expr)))
  }
  lazy val emptyModule = ReplModule(Nil, Nil)
}