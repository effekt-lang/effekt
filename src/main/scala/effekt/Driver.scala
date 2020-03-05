package effekt

// Adapted from
//   https://bitbucket.org/inkytonik/kiama/src/master/extras/src/test/scala/org/bitbucket/inkytonik/kiama/example/oberon0/base/Driver.scala

import effekt.source.{ ModuleDecl, Tree }
import effekt.namer.{ Namer, Environment }
import effekt.typer.Typer
import effekt.evaluator.Evaluator
import effekt.source.Id
import effekt.core.JavaScript
import effekt.core.{ LiftInference, Transformer }
import effekt.util.messages.{ FatalPhaseError, MessageBuffer }
import effekt.symbols.{ Effectful, Fun, Symbol, moduleFile }
import org.bitbucket.inkytonik.kiama.util.Messaging.Messages
import org.bitbucket.inkytonik.kiama.output.PrettyPrinterTypes.Document
import org.bitbucket.inkytonik.kiama.parsing.ParseResult
import org.bitbucket.inkytonik.kiama.util._
import org.rogach.scallop._

import scala.collection.mutable

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

case class CompilationUnit(
  source: Source,
  module: ModuleDecl,
  types: Memoiser[Fun, Effectful],
  symbols: Memoiser[Id, Symbol],
  exports: Environment,
  messages: Messages
)


trait Driver extends CompilerWithConfig[Tree, ModuleDecl, EffektConfig] {

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

  // - tries to find a file in the workspace, that matches the import path
  // - if compile is enabled, it will use publishProduct to write the compiled js file
  // - CompilationUnits are cached by path
  val compilationCache = mutable.Map.empty[String, CompilationUnit]

  def resolveInclude(modulePath: String, path: String, config: EffektConfig): String = {
    val p = new File(modulePath).toPath.getParent.resolve(path).toFile

    if (!p.exists()) { sys error s"Missing include: ${p}" }
    FileSource(p.getCanonicalPath).content
  }

  def resolve(path: String, config: EffektConfig): Either[CompilationUnit, Messages] =
    compilationCache.get(path).map(cu => Left(cu)).getOrElse {
      val source = findSource(path, config).getOrElse { sys error s"Cannot find source for $path" }
      makeast(source, config) match {
        case Left(ast) => frontend(source, ast, config) match {
          case Left(cu) =>
            compilationCache.update(path, cu)
            if (config.compile()) { backend(cu, config) }
            Left(cu)
          case Right(msgs) => Right(msgs)
        }
        case Right(msgs) => Right(msgs)
      }
    }

  def findSource(path: String, config: EffektConfig): Option[Source] = {
    val filename = path + ".effekt"
    config.includes().map { p => p.toPath.resolve(filename).toFile }.collectFirst {
      case file if file.exists => FileSource(file.getCanonicalPath)
    }
  }

  def frontend(source: Source, ast: ModuleDecl, config: EffektConfig): Either[CompilationUnit, Messages] = {

    /**
     * The different phases
     */
    object namer extends Namer(this, config)
    object typer extends Typer(namer.symbolTable)
    object messageBuffer extends MessageBuffer

    try {
      val env = namer.run(source.name, ast, messageBuffer)
      typer.run(ast, env, messageBuffer)

      if (messageBuffer.hasErrors) {
        Right(messageBuffer.get)
      } else {
        Left(CompilationUnit(source, ast, typer.types, namer.symbolTable, env, messageBuffer.get))
      }
    } catch {
      case FatalPhaseError(msg, reporter) =>
        reporter.error(msg)
        Right(messageBuffer.get)
    }
  }

  def copyPrelude(config: EffektConfig): Unit = {
    val preludeFile = config.outputPath().toPath.resolve("effekt.js").toFile
    if (!preludeFile.exists()) {
      println("Copying prelude to " + preludeFile.getCanonicalPath)
      val prelude = scala.io.Source.fromResource("effekt.js").getLines.mkString("\n")
      IO.createFile(preludeFile.getCanonicalPath, prelude)
    }
  }

  def backend(unit: CompilationUnit, config: EffektConfig): Unit = {
    object transformer extends Transformer
    object js extends JavaScript
    val translated = transformer.run(unit)

    val out = config.outputPath().toPath.resolve(moduleFile(unit.module.path)).toFile
    println("Writing compiled Javascript to " + out)
    IO.createFile(out.getCanonicalPath, js.format(translated).layout)
  }


  def process(source: Source, ast: ModuleDecl, config: EffektConfig) =
    frontend(source, ast, config) match {
      case Left(unit) if config.compile() =>
        copyPrelude(config)
        backend(unit, config)
      case Left(unit) =>
        object evaluator extends Evaluator(compilationCache)
        evaluator.run(unit, config.output())
      case Right(msgs) =>
        messaging.report(source, msgs, config.output())
    }

  def format(m: ModuleDecl) : Document = ???
}

object MyDriver extends Driver