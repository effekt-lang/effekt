package effekt

// Adapted from
//   https://bitbucket.org/inkytonik/kiama/src/master/extras/src/test/scala/org/bitbucket/inkytonik/kiama/example/oberon0/base/Driver.scala

import effekt.source.{ ModuleDecl, Tree }
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

  def frontend(source: Source, ast: ModuleDecl, context: CompilerContext): Either[Messages, CompilationUnit] = {

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
        Left(buffer.get)
      } else {
        Right(CompilationUnit(source, ast, exports, buffer.get))
      }
    } catch {
      case FatalPhaseError(msg, reporter) =>
        reporter.error(msg)
        Left(buffer.get)
    }
  }

  def backend(unit: CompilationUnit, context: CompilerContext): Unit = {
    object transformer extends Transformer
    object js extends JavaScript

    val translated = transformer.run(unit, context)
    // TODO report errors here

    val out = context.config.outputPath().toPath.resolve(moduleFile(unit.module.path)).toFile
    println("Writing compiled Javascript to " + out)
    IO.createFile(out.getCanonicalPath, js.format(translated).layout)
  }

  // this is a hack to experiment with server mode
  var context: CompilerContext = null

  def process(source: Source, ast: ModuleDecl, config: EffektConfig): Unit = {

    context = CompilerContext(ast, config, source => process(source, context))

    context.populate(builtins.rootTerms.values)
    process(source, ast, context) match {
      case Right(unit) =>
        if (config.compile()) {
          copyPrelude(config)
        }

        if (!config.server()) {
          object evaluator extends Evaluator
          evaluator.run(unit, context)
        }
      case Left(msgs) =>
        clearSyntacticMessages(source, config)
        clearSemanticMessages(source, config)
        report(source, msgs, config)
    }
  }

  def process(source: Source, ast: ModuleDecl, context: CompilerContext): Either[Messages, CompilationUnit] = {
    frontend(source, ast, context) match {
      case Right(unit) if context.config.compile() =>
        backend(unit, context)
        Right(unit)
      case result => result
    }
  }

  def process(source: Source, context: CompilerContext): Either[Messages, CompilationUnit] =
    // for some reason Kiama uses the Either the other way around.
    makeast(source, context.config) match {
      case Left(ast) => process(source, ast, context)
      case Right(msgs) => Left(msgs)
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

trait LSPServer extends Driver {

  import effekt.symbols._
  import effekt.source.{ Reference, Definition }
  type EffektTree = kiama.relation.Tree[Tree, ModuleDecl]

  def getInfoAt(position: Position): Option[(Vector[Tree], CompilationUnit)] = for {
    unit <- context.resolve(position.source).toOption
    tree = new EffektTree(unit.module)
    nodes = positions.findNodesContaining(tree.nodes, position)
  } yield (nodes, unit)

  override def getDefinition(position: Position): Option[Tree] = for {
    (trees, unit) <- getInfoAt(position)
    id <- trees.collectFirst { case id: source.Id => id  }
    decl <- context.lookup(id) match {
      case u: UserFunction => Some(u.decl)
      case u: Binder => Some(u.decl)
      case _ => None
    }
  } yield decl

  override def getHover(position : Position): Option[String] = for {
    (trees, unit) <- getInfoAt(position)
    sym <- trees.collectFirst {
      case d: Definition => context.get(d)
      case r: Reference => context.get(r)
    }
    tpe = sym match {
      case s: ValueSymbol => context.valueType(s)
      case b: BlockSymbol => context.blockType(b)
    }
  } yield tpe.toString

}

object Server extends LSPServer