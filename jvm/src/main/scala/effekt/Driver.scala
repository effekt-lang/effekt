package effekt

// Adapted from
//   https://bitbucket.org/inkytonik/kiama/src/master/extras/src/test/scala/org/bitbucket/inkytonik/kiama/example/oberon0/base/Driver.scala

import effekt.source.{ ModuleDecl, Tree }
import effekt.evaluator.Evaluator
import effekt.core.{ JavaScript, Transformer }
import effekt.util.messages.FatalPhaseError
import effekt.symbols.{ Module, builtins }
import effekt.context.Context
import org.bitbucket.inkytonik.kiama
import kiama.util.Messaging.Messages
import kiama.output.PrettyPrinterTypes.Document
import kiama.parsing.ParseResult
import kiama.util._
import org.rogach.scallop._
import java.io.File

import effekt.namer.Namer
import effekt.typer.Typer



class EffektConfig(args : Seq[String]) extends REPLConfig(args) {
  lazy val compile: ScallopOption[Boolean] = toggle(
    "compile",
    descrYes = "Compile the Effekt program to JavaScript",
    descrNo = "Run the effekt program in the interpreter",
    default = Some(false)
  )

  lazy val outputPath: ScallopOption[File] = opt[File](
    "out",
    descr = "Path to write generated JavaScript files to (defaults to ./out)",
    default = Some(new File("./out")),
    required = false
  )

  lazy val includes: ScallopOption[List[File]] = opt[List[File]](
    "includes",
    descr = "Path to consider for includes (can be set multiple times)",
    default = Some(List(new File("."))),
    noshort = true
  )

  validateFilesIsDirectory(includes)
}


trait Driver extends CompilerWithConfig[Tree, ModuleDecl, EffektConfig] { driver =>

  val name = "effekt"

  // Frontend phases
  // ===============
  object parser extends Parser(positions)
  object namer extends Namer
  object typer extends Typer

  // Compiler context
  // ================
  // We always only have one global instance of CompilerContext
  object context extends Context {

    /**
     * This is used to resolve imports
     */
    override def process(source: Source): Module =
      driver.frontend(source)(this) match {
        case Right(res) =>
          driver.backend(res)(this)
          res
        case Left(msgs) =>
          report(source, msgs, config)
          abort(s"Error processing dependency: ${source.name}")
      }

    /**
     * This is used by the LSP Server to perform type checking.
     */
    override def frontend(source: Source): Option[Module] =
      driver.frontend(source)(this).toOption

    populate(builtins.rootTerms.values)
  }

  // no file names are given, run REPL
  override def run(config: EffektConfig): Unit =
    if (config.filenames().isEmpty && !config.server()) {
      new Repl(this).run(config)
    } else {
      super.run(config)
    }

  override def createConfig(args : Seq[String]) =
    new EffektConfig(args)

  override def parse(source: Source): ParseResult[ModuleDecl] =
    parser.parseAll(parser.program, source)

  /**
   * Main entry to the compiler, invoked by Kiama after parsing with `parse`
   */
  override def process(source: Source, ast: ModuleDecl, config: EffektConfig): Unit = {

    implicit val C = context

    context.setup(ast, config)

    process(source, ast) match {
      case Right(unit) =>
        if (!config.server() && !config.compile()) {
          object evaluator extends Evaluator
          evaluator.run(unit)
        }
        report(source)

      case Left(msgs) =>
        clearSyntacticMessages(source, config)
        clearSemanticMessages(source, config)
        report(source, msgs, config)
    }
  }

  def process(source: Source)(implicit C: Context): Either[Messages, Module] =
    // for some reason Kiama uses the Either the other way around.
    makeast(source, C.config) match {
      case Left(ast) => process(source, ast)
      case Right(msgs) => Left(msgs)
    }

  def process(source: Source, ast: ModuleDecl)(implicit C: Context): Either[Messages, Module] =
    frontend(source, ast) match {
      case Right(unit) if C.config.compile() || C.config.server() =>
        backend(unit)
        Right(unit)
      case result => result
    }

  /**
   * Frontend: Parser -> Namer -> Typer
   */
  def frontend(source: Source)(implicit C: Context): Either[Messages, Module] =
    makeast(source, C.config) match {
      case Left(ast) => frontend(source, ast)
      case Right(msgs) => Left(msgs)
    }

  /**
   * Frontend: Namer -> Typer
   */
  def frontend(source: Source, ast: ModuleDecl)(implicit C: Context): Either[Messages, Module] = {

    val buffer = C.buffer

    try {
      val mod = namer.run(source, ast)
      typer.run(ast, mod)

      if (buffer.hasErrors) {
        Left(buffer.get)
      } else {
        Right(mod)
      }
    } catch {
      case FatalPhaseError(msg, reporter) =>
        reporter.error(msg)
        Left(C.buffer.get)
    }
  }

  /**
   * Backend: Effekt -> Core -> JavaScript
   */
  def backend(unit: Module)(implicit C: Context): Unit = try {

    object transformer extends Transformer
    object js extends JavaScript
    object prettyCore extends core.PrettyPrinter

    val translated = transformer.run(unit)
    val javaScript = js.format(translated)

    if (C.config.server() && settingBool("showTarget")) {
      publishProduct(unit.source, "target", "js", javaScript)
    }

    if (C.config.server() && settingBool("showCore")) {
      publishProduct(unit.source, "target", "effekt", prettyCore.format(translated))
    }

    if (C.config.compile()) {
      val outDir = C.config.outputPath().toPath
      outDir.toFile.mkdirs
      val out = outDir.resolve(unit.outputName).toFile

      println("Writing compiled Javascript to " + out)
      IO.createFile(out.getCanonicalPath, javaScript.layout)
    }
  } catch {
    case FatalPhaseError(msg, reporter) =>
      reporter.error(msg)
      report(unit.source)
  }

  def report(in: Source)(implicit C: Context): Unit =
    report(in, C.buffer.get, C.config)

  def format(m: ModuleDecl) : Document = ???
}
