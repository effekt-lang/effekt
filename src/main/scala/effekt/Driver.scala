package effekt

// Adapted from
//   https://bitbucket.org/inkytonik/kiama/src/master/extras/src/test/scala/org/bitbucket/inkytonik/kiama/example/oberon0/base/Driver.scala

import effekt.source.{ ModuleDecl, Tree }
import effekt.namer.{ Environment, Namer }
import effekt.typer.Typer
import effekt.evaluator.Evaluator
import effekt.core.{ JavaScript, Transformer }
import effekt.util.messages.{ ErrorReporter, FatalPhaseError, MessageBuffer }
import effekt.symbols.{ builtins, TypesDB, SymbolsDB, moduleFile }
import org.bitbucket.inkytonik.kiama.util.Messaging.Messages
import org.bitbucket.inkytonik.kiama.output.PrettyPrinterTypes.Document
import org.bitbucket.inkytonik.kiama.parsing.ParseResult
import org.bitbucket.inkytonik.kiama.util._
import org.rogach.scallop._

import java.io.File

class EffektConfig(args : Seq[String]) extends Config(args) {
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

  validateFileIsDirectory(outputPath)

  lazy val includes = opt[List[File]](
    "includes",
    descr = "Path to consider for includes (can be set multiple times)",
    default = Some(List(new File("."), new File("./lib"))),
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

/**
 * The compiler context consists of
 * - configuration (immutable)
 * - symbols (mutable database)
 * - types (mutable database)
 * - error reporting (mutable focus)
 */
class CompilerContext(
  var focus: Tree,
  val config: EffektConfig,
  val process: Source => Either[CompilationUnit, Messages]
) extends ErrorReporter with TypesDB with SymbolsDB with ModuleDB {
  val buffer: MessageBuffer = new MessageBuffer

  def at[T](t: Tree)(block: => T): T = {
    val before = focus
    focus = t;
    val res = block;
    focus = before;
    res
  }
}

trait Driver extends CompilerWithConfig[Tree, ModuleDecl, EffektConfig] { driver =>

  val name = "effekt"

  override def createConfig(args : Seq[String]) = {
    new EffektConfig(args)
  }

  override def compileFile(filename: String, config: EffektConfig,
      encoding : String = "UTF-8"): Unit = {
    val output = config.output()
    val source = FileSource(filename, encoding)

    makeast(source, config) match {
        case Left(ast) =>
            process(source, ast, config)
        case Right(msgs) =>
            output.emit(messaging.formatMessages(msgs))
    }
  }

  def parse(source: Source): ParseResult[ModuleDecl] = {
    val parsers = new Parser(positions)
    parsers.parseAll(parsers.program, source)
  }

  def frontend(source: Source, ast: ModuleDecl, context: CompilerContext): Either[CompilationUnit, Messages] = {

    /**
     * The different phases
     */
    object namer extends Namer
    object typer extends Typer

    val buffer = context.buffer

    try {
      val exports = namer.run(source.name, ast, context)
      typer.run(ast, exports, context)

      // TODO improve error reporting code
      if (buffer.hasErrors) {
        Right(buffer.get)
      } else {
        Left(CompilationUnit(source, ast, exports, buffer.get))
      }
    } catch {
      case FatalPhaseError(msg, reporter) =>
        reporter.error(msg)
        Right(buffer.get)
    }
  }

  def backend(unit: CompilationUnit, context: CompilerContext): Unit = {
    object transformer extends Transformer
    object js extends JavaScript
    object messageBuffer extends MessageBuffer

    val translated = transformer.run(unit, context)
    // TODO report errors here

    val out = context.config.outputPath().toPath.resolve(moduleFile(unit.module.path)).toFile
    println("Writing compiled Javascript to " + out)
    IO.createFile(out.getCanonicalPath, js.format(translated).layout)
  }


  def process(source: Source, ast: ModuleDecl, config: EffektConfig) = {

    lazy val context: CompilerContext = CompilerContext(ast, config, source => process(source, context))

    context.populate(builtins.rootTerms.values)
    process(source, ast, context) match {
      case Left(unit) if config.compile() =>
        copyPrelude(config)
      case Left(unit) =>
        object evaluator extends Evaluator
        evaluator.run(unit, context)

      case Right(msgs) =>
        messaging.report(source, msgs, config.output())
    }
  }

  def process(source: Source, ast: ModuleDecl, context: CompilerContext): Either[CompilationUnit, Messages] = {
    frontend(source, ast, context) match {
      case Left(unit) if context.config.compile() =>
        // TODO maybe this is a problem since the unit needs to be written to the compilation cache (units)
        //      first, before running backend.
        backend(unit, context)
        Left(unit)
      case result => result
    }
  }

  def process(source: Source, context: CompilerContext): Either[CompilationUnit, Messages] =
    makeast(source, context.config) match {
      case Left(ast) => process(source, ast, context)
      case Right(msgs) => Right(msgs)
    }

  def copyPrelude(config: EffektConfig): Unit = {
    val preludeFile = config.outputPath().toPath.resolve("effekt.js").toFile
    if (!preludeFile.exists()) {
      println("Copying prelude to " + preludeFile.getCanonicalPath)
      val prelude = scala.io.Source.fromResource("effekt.js").getLines.mkString("\n")
      IO.createFile(preludeFile.getCanonicalPath, prelude)
    }
  }

  def format(m: ModuleDecl) : Document = ???
}

object MyDriver extends Driver