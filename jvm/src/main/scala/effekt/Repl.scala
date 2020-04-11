package effekt

import effekt.source._
import effekt.symbols.{ BlockSymbol, Module, ValueSymbol }
import effekt.util.{ AmmoniteConsole, ColoredMessaging }
import org.bitbucket.inkytonik.kiama
import kiama.util.Messaging.{ Messages, message }
import kiama.util.{ Console, ParsingREPLWithConfig, Source, StringSource }
import kiama.parsing.{ NoSuccess, ParseResult, Success }

import scala.annotation.tailrec

// for now we only take expressions
class Repl(driver: Driver) extends ParsingREPLWithConfig[Tree, EffektConfig] {

  var module: ReplModule = emptyModule

  override val messaging = new ColoredMessaging(positions)

  override val prompt = "\neffekt>"

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
    def printHelp(): Unit = {
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
           |:quit (:q)                quit this REPL""".stripMargin
      )
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

  override def processlines(config: EffektConfig): Unit = readEvalPrint(config)

  @tailrec final def readEvalPrint(config: EffektConfig): EffektConfig = AmmoniteConsole.readLineOption(prompt) match {
    case None => config
    case Some(line) => processline(StringSource(line), AmmoniteConsole, config) match {
      case None         => config
      case Some(config) => readEvalPrint(config)
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

    runFrontend(source, module.make(UnitLit()), config) { cu =>
      val ctx = driver.context

      module.definitions.foreach {
        case u: Def =>
          val sym = ctx.symbolOf(u)
          out.emitln(DeclPrinter(sym)(driver.context))
      }
    }
  }

  // TODO refactor and clean this up
  def typecheck(source: Source, config: EffektConfig): Unit =
    parse(source) match {
      case Success(e: Expr, _) =>
        runFrontend(source, module.make(e), config) { mod =>
          val mainSym = mod.terms("main").head
          val mainTpe = driver.context.blockTypeOf(mainSym)
          config.output().emitln(mainTpe.ret)
        }

      case Success(other, _) =>
        config.output().emitln("Can only show type of expressions")

      // this is usually encapsulated in REPL.processline
      case res: NoSuccess =>
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
      runCompiler(source, module.makeEval(e), config)

    case i: Import if !module.contains(i) =>
      val extendedImports = module + i
      val decl = extendedImports.make(UnitLit())

      runFrontend(source, decl, config) { cu =>
        module = extendedImports
      }

    case d: Def =>
      val extendedDefs = module + d
      val decl = extendedDefs.make(UnitLit())
      runFrontend(source, decl, config) { cu =>
        module = extendedDefs

        // try to find the symbol for the def to print the type
        (driver.context.symbolOf(d) match {
          case v: ValueSymbol =>
            Some(driver.context.valueTypeOf(v))
          case b: BlockSymbol =>
            Some(driver.context.blockTypeOf(b))
          case t =>
            None
        }) map { tpe =>
          config.output().emitln(tpe)
        }
      }

    case _ => ()
  }

  def runCompiler(source: Source, ast: ModuleDecl, config: EffektConfig): Unit = {
    implicit val context = driver.context
    context.setup(config)

    for {
      mod <- driver.compile(ast, source)
    } driver.eval(mod)

    report(source, driver.context.buffer.get, config)
  }

  def runFrontend(source: Source, ast: ModuleDecl, config: EffektConfig)(f: Module => Unit): Unit = {
    driver.context.setup(config)
    driver.frontend(ast, source)(driver.context) map { f } getOrElse {
      report(source, driver.context.buffer.get, driver.context.config)
    }
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
    def +(d: Def) = {
      // drop all equally named definitions for now.
      val otherDefs = definitions.filterNot { other => other.id.name == d.id.name }
      copy(definitions = otherDefs :+ d)
    }
    def +(i: Import) = copy(imports = imports :+ i)

    def contains(im: Import) = imports.exists { other => im.path == other.path }

    /**
     * Create a module declaration using the given expression as body of main
     */
    def make(expr: Expr): ModuleDecl = {

      val body = Return(expr)

      ModuleDecl("lib/interactive", Import("effekt") :: imports,
        definitions :+ FunDef(IdDef("main"), Nil, List(ValueParams(Nil)), None,
          body))
    }

    def make2(expr: Expr): ModuleDecl = {

      // partition into toplevel definitions and into those that go into main:
      val (toplevel, local) = definitions.partition {
        case _: ValDef => false
        case _: FunDef => false
        case _         => true
      }

      // all definitions are moved to the main function, so that value bindings work
      val body = local.foldRight[Stmt](Return(expr)) { case (d, body) => DefStmt(d, body) }

      ModuleDecl("lib/interactive", Import("effekt") :: imports,
        toplevel :+ FunDef(IdDef("main"), Nil, List(ValueParams(Nil)), None,
          body))
    }

    def makeEval(expr: Expr): ModuleDecl =
      make(Call(IdRef("println"), Nil, List(ValueArgs(List(expr)))))
  }
  lazy val emptyModule = ReplModule(Nil, Nil)
}
