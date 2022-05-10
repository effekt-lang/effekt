package effekt

import effekt.source._
import effekt.context.{ Context, IOModuleDB }
import effekt.symbols.{ BlockSymbol, DeclPrinter, Module, ValueSymbol }
import effekt.util.{ ColoredMessaging, Highlight, VirtualSource }
import effekt.util.Version.effektVersion
import kiama.util.Messaging.{ Messages, message }
import kiama.util.{ Console, REPL, Source, StringSource }
import kiama.parsing.{ NoSuccess, ParseResult, Success }

class Repl(driver: Driver) extends REPL[Tree, EffektConfig] {

  private implicit lazy val context: Context with IOModuleDB = driver.context

  override val messaging = new ColoredMessaging(positions)

  val logo =
    """|  _____     ______  __  __     _    _
       | (_____)   |  ____|/ _|/ _|   | |  | |
       |   ___     | |__  | |_| |_ ___| | _| |_
       |  (___)    |  __| |  _|  _/ _ \ |/ / __|
       |  _____    | |____| | | ||  __/   <| |_
       | (_____)   |______|_| |_| \___|_|\_\\__|
       |""".stripMargin

  override val banner = logo +
    s"""|
        | Welcome to the Effekt interpreter (v$effektVersion). Enter a top-level definition, or
        | an expression to evaluate.
        |
        | To print the available commands, enter :help
        |""".stripMargin

  override def createConfig(args: Seq[String]) = driver.createConfig(args)

  /**
   * Adapting Kiama REPL's driver to work with an already processed config.
   *
   * Called by the Driver if no arguments are provided to the Effekt binary
   */
  def run(config: EffektConfig): Unit = {
    val out = config.output()
    out.emitln(banner)
    usingCommandHistory(config) {
      processlines(config)
      out.emitln()
    }
  }

  /**
   * Use the special `repl` nonterminal to process the input. It recognizes expressions, statements
   * and everything else that can occur on the top-level.
   */
  override def parse(source: Source): ParseResult[Tree] = {
    val parsers = new EffektParsers(positions)
    parsers.parseAll(parsers.repl, source)
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

    /**
     * Command `help`: Prints help about the available commands.
     */
    def help(): Unit = {
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

    /**
     * Command `:reset` -- Reset the virtual module the Repl operates on
     */
    def reset() = {
      module = emptyModule
    }

    /**
     * Command `:status` -- Prints help about the available commands
     */
    def status(config: EffektConfig): Unit = {
      import symbols._

      module.imports.foreach { im =>
        outputCode(s"import ${im.path}", config)
      }
      output.emitln("")

      runFrontend(StringSource(""), module.make(UnitLit()), config) { cu =>
        module.definitions.foreach {
          case u: Def =>
            outputCode(DeclPrinter(context.symbolOf(u)), config)
        }
      }
    }

    /**
     * Command `:type` -- runs the frontend (and typechecker) on the given expression
     */
    def typecheck(source: Source, config: EffektConfig): Unit =
      parse(source) match {
        case Success(e: Term, _) =>
          runFrontend(source, module.make(e), config) { mod =>
            // TODO this is a bit ad-hoc
            val mainSym = mod.terms("main").head
            val mainTpe = context.blockTypeOf(mainSym)
            output.emitln(mainTpe.result)
          }

        case Success(other, _) =>
          output.emitln("Can only show type of expressions")

        // this is usually encapsulated in REPL.processline
        case res: NoSuccess =>
          val pos = res.next.position
          positions.setStart(res, pos)
          positions.setFinish(res, pos)
          val messages = message(res, res.message)
          report(source, messages, config)
      }

    source.content match {
      case Command(":status", _) =>
        status(config)
        Some(config)

      case Command(":reset", _) =>
        reset()
        Some(config)

      case Command(":l", path) =>
        val src = StringSource(s"import $path", source.name)
        super.processline(src, console, config)
        Some(config)

      case Command(":imports", _) =>
        output.emitln(module.imports.map { i => i.path }.mkString("\n"))
        Some(config)

      case Command(":help", _) | Command(":h", _) =>
        help()
        Some(config)

      case Command(":quit", _) | Command(":q", _) =>
        None

      case Command(cmd, expr) if cmd == ":type" || cmd == ":t" =>
        typecheck(StringSource(expr, source.name), config)
        Some(config)

      case Command(cmd, _) =>
        output.emitln(s"Unknown command ${cmd}, enter :help for a list of available commands")
        Some(config)

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
    case e: Term =>
      runCompiler(source, module.makeEval(e), config)

    case i: Import =>
      val extendedImports = module + i
      val output = config.output()

      context.setup(config)
      val src = context.findSource(i.path).getOrElse {
        output.emitln(s"Cannot find source for import ${i.path}")
        return
      }

      runParsingFrontend(src, config) { cu =>
        output.emitln(s"Successfully imported ${i.path}\n")
        output.emitln(s"Imported Types\n==============")
        cu.types.toList.sortBy { case (n, _) => n }.collect {
          case (name, sym) if !sym.synthetic =>
            outputCode(DeclPrinter(sym), config)
        }
        output.emitln(s"\nImported Functions\n==================")
        cu.terms.toList.sortBy { case (n, _) => n }.foreach {
          case (name, syms) =>
            syms.collect {
              case sym if !sym.synthetic =>
                outputCode(DeclPrinter(sym), config)
            }
        }
        module = extendedImports
      }

    case d: Def =>
      val extendedDefs = module + d
      val decl = extendedDefs.make(UnitLit())
      runFrontend(source, decl, config) { cu =>
        module = extendedDefs

        // try to find the symbol for the def to print the type
        (context.symbolOf(d) match {
          case v: ValueSymbol =>
            Some(context.valueTypeOf(v))
          case b: BlockSymbol =>
            Some(context.blockTypeOf(b))
          case t =>
            None
        }) map { tpe =>
          outputCode(s"${d.id.name}: ${tpe}", config)
        }
      }

    case _ => ()
  }

  private def runCompiler(source: Source, ast: ModuleDecl, config: EffektConfig): Unit =
    driver.compileSource(VirtualSource(ast, source), config)

  private def runFrontend(source: Source, ast: ModuleDecl, config: EffektConfig)(f: Module => Unit): Unit = {
    context.setup(config)
    val src = VirtualSource(ast, source)
    context.runFrontend(src) map { f } getOrElse {
      report(source, context.buffer.get, context.config)
    }
  }

  private def runParsingFrontend(source: Source, config: EffektConfig)(f: Module => Unit): Unit = {
    context.setup(config)
    context.runFrontend(source) map { f } getOrElse {
      report(source, context.buffer.get, context.config)
    }
  }

  object Command {
    import scala.util.matching.Regex

    val command = ":[\\w]+".r
    def unapply(str: String): Option[(String, String)] = command.findPrefixMatchOf(str) map {
      m => (m.matched, str.substring(m.end).trim)
    }
  }

  def outputCode(code: String, config: EffektConfig): Unit =
    config.output().emitln(Highlight(code))

  /**
   * Enables persistent command history on JLine
   */
  def usingCommandHistory[T](config: EffektConfig)(block: => T): T = {
    import kiama.util.JLineConsole
    import effekt.util.paths._
    import jline.console.history.FileHistory

    config.console() match {
      case c: JLineConsole.type =>
        val historyFile = file(System.getProperty("user.home")) / ".effekt_history"
        val history = new FileHistory(historyFile.toFile)
        c.reader.setHistory(history)
        c.reader.setHistoryEnabled(true)

        try { block } finally { history.flush() }
      case _ => block
    }
  }

  private var module: ReplModule = emptyModule

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
    def +(i: Import) = copy(imports = imports.filterNot { _.path == i.path } :+ i)

    def contains(im: Import) = imports.exists { other => im.path == other.path }

    /**
     * Create a module declaration using the given expression as body of main
     */
    def make(expr: Term): ModuleDecl = {

      val body = Return(expr)

      ModuleDecl("interactive", Import("effekt") :: imports,
        definitions :+ FunDef(IdDef("main"), Nil, List(ValueParams(Nil)), None,
          body))
    }

    def makeEval(expr: Term): ModuleDecl =
      make(Call(IdTarget(IdRef("println")), Nil, List(ValueArgs(List(expr)))))
  }
  lazy val emptyModule = ReplModule(Nil, Nil)
}
