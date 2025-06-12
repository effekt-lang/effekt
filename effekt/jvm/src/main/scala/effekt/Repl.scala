package effekt

import effekt.lexer.Token
import effekt.source.*
import effekt.context.{Context, IOModuleDB}
import effekt.symbols.{BlockSymbol, DeclPrinter, ErrorMessageInterpolator, Module, ValueSymbol, isSynthetic}
import effekt.util.{AnsiColoredMessaging, AnsiHighlight, VirtualSource, getOrElseAborting}
import effekt.util.messages.EffektError
import effekt.util.Version.effektVersion
import kiama.util.{Console, REPL, Range, Source, StringSource}
import kiama.parsing.{NoSuccess, ParseResult, Success, Input}

class Repl(driver: Driver) extends REPL[Tree, EffektConfig, EffektError] {

  private implicit lazy val context: Context with IOModuleDB = driver.context

  val messaging = new AnsiColoredMessaging

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
    val lexer = effekt.lexer.Lexer(source)
    val tokens = lexer.lex()
    val parser = RecursiveDescent(context.positions, tokens, source)
    parser.parseRepl(Input(source, 0))
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

      module.includes.foreach { im =>
        outputCode(s"import ${im.path}", config)
      }
      output.emitln("")

      runFrontend(StringSource(""), module.make(StringSource(""), UnitLit(Span.missing)), config) { cu =>
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
          runFrontend(source, module.make(source, e), config) { mod =>
            // TODO this is a bit ad-hoc
            val mainSym = mod.exports.terms("main").head
            val mainTpe = context.functionTypeOf(mainSym)
            output.emitln(pp"${mainTpe.result}")
          }

        case Success(other, _) =>
          output.emitln("Can only show type of expressions")

        // this is usually encapsulated in REPL.processline
        case res: NoSuccess =>
          val pos = res.next.position
          val messages = Vector(messaging.message(Range(pos, pos), res.message))
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
        output.emitln(module.includes.map { i => i.path }.mkString("\n"))
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
      runCompiler(source, module.makeEval(source, e), config)

    case i: Include =>
      val extendedIncludes = module + i
      val output = config.output()

      context.setup(config)

      val src = context.findSource(i.path).getOrElseAborting {
        output.emitln(s"Cannot find source for import ${i.path}")
        return
      }

      runParsingFrontend(src, config) { cu =>
        output.emitln(s"Successfully imported ${i.path}\n")
        output.emitln(s"Imported Types\n==============")
        cu.exports.types.toList.sortBy { case (n, _) => n }.collect {
          case (name, sym) if !sym.isSynthetic =>
            outputCode(DeclPrinter(sym), config)
        }
        output.emitln(s"\nImported Functions\n==================")
        cu.exports.terms.toList.sortBy { case (n, _) => n }.foreach {
          case (name, syms) =>
            syms.collect {
              case sym if !sym.isSynthetic =>
                outputCode(DeclPrinter(sym), config)
            }
        }
        module = extendedIncludes
      }

    case d: Def =>
      val extendedDefs = module + d
      val decl = extendedDefs.make(source, UnitLit(Span.missing))

      runFrontend(source, decl, config) { cu =>
        module = extendedDefs

        // try to find the symbol for the def to print the type
        (context.symbolOption(d.id) match {
          case Some(v: ValueSymbol) =>
            Some(context.valueTypeOf(v))
          case Some(b: BlockSymbol) =>
            Some(context.blockTypeOf(b))
          case t =>
            None
        }) map { tpe =>
          outputCode(pp"${d.id}: ${tpe}", config)
        }
      }

    case _ => ()
  }

  private def runCompiler(source: Source, ast: ModuleDecl, config: EffektConfig): Unit =
    driver.compileSource(VirtualSource(ast, source), config)

  private def runFrontend(source: Source, ast: ModuleDecl, config: EffektConfig)(f: Module => Unit): Unit = {
    context.setup(config)
    val src = VirtualSource(ast, source)
    context.compiler.runFrontend(src) map { f } getOrElse {
      report(source, context.messaging.buffer, context.config)
    }
  }

  private def runParsingFrontend(source: Source, config: EffektConfig)(f: Module => Unit): Unit = {
    context.setup(config)
    context.compiler.runFrontend(source) map { f } getOrElse {
      report(source, context.messaging.buffer, context.config)
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
    config.output().emitln(AnsiHighlight(code))

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
    includes: List[Include]
  ) {
    def +(d: Def) = {
      // drop all equally named definitions for now.
      val otherDefs = definitions.filterNot { other => other.id.name == d.id.name }
      copy(definitions = otherDefs :+ d)
    }
    def +(i: Include) = copy(includes = includes.filterNot { _.path == i.path } :+ i)

    def contains(im: Include) = includes.exists { other => im.path == other.path }

    /**
     * Create a module declaration using the given expression as body of main
     */
    def make(source: Source, expr: Term): ModuleDecl = {

      val body = Return(expr, expr.span.synthesized)
      val fakeSpan = Span(source, 0, 0, origin = Origin.Synthesized)
      val fullSpan = Span(source, 0, source.content.length, origin = Origin.Synthesized)
      ModuleDecl("interactive", includes,
        definitions :+ FunDef(IdDef("main", fakeSpan), Many.empty(fakeSpan), Many.empty(fakeSpan), Many.empty(fakeSpan), Maybe.None(fakeSpan),
          body, None, fullSpan), None, fullSpan)
    }

    def makeEval(source: Source, expr: Term): ModuleDecl = {
      val fakeSpan = Span(source, 0, 0, origin = Origin.Synthesized)
      make(source, Call(IdTarget(IdRef(List(), "println", fakeSpan)), Nil, List(expr), Nil, fakeSpan))
    }
  }
  lazy val emptyModule = ReplModule(Nil, Nil)
}
