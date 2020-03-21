package effekt

// Adapted from
//   https://bitbucket.org/inkytonik/kiama/src/master/extras/src/test/scala/org/bitbucket/inkytonik/kiama/example/oberon0/base/Driver.scala

import effekt.source.{ Expr, ModuleDecl, Tree }
import effekt.namer.{ Environment, Namer }
import effekt.typer.Typer
import effekt.evaluator.Evaluator
import effekt.core.{ JavaScript, Transformer }
import effekt.util.messages.{ ErrorReporter, FatalPhaseError, MessageBuffer }
import effekt.symbols.{ builtins, moduleFile }
import org.bitbucket.inkytonik.kiama
import kiama.util.Messaging.Messages
import kiama.output.PrettyPrinterTypes.Document
import kiama.parsing.ParseResult
import kiama.util._
import org.rogach.scallop._
import java.io.File

import context.CompilerContext

class EffektConfig(args : Seq[String]) extends REPLConfig(args) {
  lazy val compile = toggle(
    "compile",
    descrYes = "Compile the Effekt program to JavaScript",
    descrNo = "Run the effekt program in the interpreter",
    default = Some(false)
  )

  lazy val outputPath = opt[File](
    "out",
    descr = "Path to write generated JavaScript files to (defaults to ./out)",
    default = Some(new File("./out")),
    required = false
  )

  lazy val includes = opt[List[File]](
    "includes",
    descr = "Path to consider for includes (can be set multiple times)",
    default = Some(List(new File("."))),
    noshort = true
  )

  validateFilesIsDirectory(includes)
}

/**
 * The result of running the frontend on a module.
 * Symbols and types are stored globally in CompilerContext.
 */
case class CompilationUnit(
  source: Source,
  module: ModuleDecl,
  exports: Environment,
  messages: Messages
)


trait Driver extends CompilerWithConfig[Tree, ModuleDecl, EffektConfig] { driver =>

  val name = "effekt"

  // We always only have one global instance of CompilerContext
  object context extends CompilerContext {

    override def process(source: Source): CompilationUnit =
      driver.frontend(source, this) match {
        case Right(res) => res
        case Left(msgs) =>
          report(source, msgs, config)
          abort(s"Error processing dependency: ${source.name}")
      }

    override def tryProcess(source: Source): Option[CompilationUnit] =
      driver.frontend(source, this).toOption

    populate(builtins.rootTerms.values)
  }


  override def createConfig(args : Seq[String]) =
    new EffektConfig(args)

  override def parse(source: Source): ParseResult[ModuleDecl] = {
    val parsers = new Parser(positions)
    parsers.parseAll(parsers.program, source)
  }

  /**
   * Main entry to the compiler, invoked by Kiama after parsing with `parse`
   */
  override def process(source: Source, ast: ModuleDecl, config: EffektConfig): Unit = {

    context.setup(ast, config)

    process(source, ast, context) match {
      case Right(unit) =>
        if (!config.server() && !config.compile()) {
          object evaluator extends Evaluator
          evaluator.run(unit, context)
        }

        report(source, unit.messages, config)
      case Left(msgs) =>
        clearSyntacticMessages(source, config)
        clearSemanticMessages(source, config)
        report(source, msgs, config)
    }
  }

  def process(source: Source, context: CompilerContext): Either[Messages, CompilationUnit] =
    // for some reason Kiama uses the Either the other way around.
    makeast(source, context.config) match {
      case Left(ast) => process(source, ast, context)
      case Right(msgs) => Left(msgs)
    }

  def process(source: Source, ast: ModuleDecl, context: CompilerContext): Either[Messages, CompilationUnit] =
    frontend(source, ast, context) match {
      case Right(unit) if context.config.compile() || context.config.server() =>
        backend(unit, context)
        Right(unit)
      case result => result
    }

  /**
   * Frontend: Parser -> Namer -> Typer
   */
  def frontend(source: Source, context: CompilerContext): Either[Messages, CompilationUnit] =
    makeast(source, context.config) match {
      case Left(ast) => frontend(source, ast, context)
      case Right(msgs) => Left(msgs)
    }

  /**
   * Frontend: Namer -> Typer
   */
  def frontend(source: Source, ast: ModuleDecl, context: CompilerContext): Either[Messages, CompilationUnit] = {

    object namer extends Namer
    object typer extends Typer

    val buffer = context.buffer

    try {
      val exports = namer.run(source, ast, context)
      typer.run(ast, exports, context)

      if (buffer.hasErrors) {
        Left(buffer.get)
      } else {
        Right(CompilationUnit(source, ast, exports, buffer.get))
      }
    } catch {
      case FatalPhaseError(msg, reporter) =>
        reporter.error(msg)
        Left(context.buffer.get)
    }
  }

  /**
   * Backend: Evaluator or Translator
   */
  def backend(unit: CompilationUnit, context: CompilerContext): Unit = try {
    object transformer extends Transformer
    object js extends JavaScript
    object prettyCore extends core.PrettyPrinter

    val config = context.config

    val translated = transformer.run(unit, context)
    val javaScript = js.format(translated)

    if (config.server() && settingBool("showTarget")) {
      publishProduct(unit.source, "target", "js", javaScript)
    }

    if (config.server() && settingBool("showCore")) {
      publishProduct(unit.source, "target", "effekt", prettyCore.format(translated))
    }

    if (config.compile()) {
      val outDir = config.outputPath().toPath
      outDir.toFile.mkdirs
      val out = outDir.resolve(moduleFile(unit.module.path)).toFile

      println("Writing compiled Javascript to " + out)
      IO.createFile(out.getCanonicalPath, javaScript.layout)
    }
  } catch {
    case FatalPhaseError(msg, reporter) =>
      reporter.error(msg)
      report(unit.source, context.buffer.get, context.config)
  }

  def format(m: ModuleDecl) : Document = ???
}
