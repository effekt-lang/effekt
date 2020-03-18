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
      val exports = namer.run(source, ast, context)
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
      val out = config.outputPath().toPath.resolve(moduleFile(unit.module.path)).toFile
      println("Writing compiled Javascript to " + out)
      IO.createFile(out.getCanonicalPath, javaScript.layout)
    }
  }

  // this is a hack to experiment with server mode
  object context extends CompilerContext {
    override def process(source: Source): Either[Messages, CompilationUnit] = driver.process(source, this)

    populate(builtins.rootTerms.values)
  }

  def process(source: Source, ast: ModuleDecl, config: EffektConfig): Unit = {
    
//    if (config.compile()) {
//      copyPrelude(config)
//    }

    context.setup(ast, config)

    process(source, ast, context) match {
      case Right(unit) =>
        if (!config.server()) {
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

  def process(source: Source, ast: ModuleDecl, context: CompilerContext): Either[Messages, CompilationUnit] = {
    frontend(source, ast, context) match {
      case Right(unit) if context.config.compile() || context.config.server() =>
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

  // TODO create temp folder, copy files from JAR/lib to the temp folder and
  //      add folder to the config
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
  import effekt.source.{ Reference, Definition, Id, Literal }

  import org.eclipse.lsp4j.{ Location, Range => LSPRange }

  type EffektTree = kiama.relation.Tree[Tree, ModuleDecl]

  def getInfoAt(position: Position): Option[(Vector[Tree], CompilationUnit)] = for {
    unit <- context.resolve(position.source).toOption
    tree = new EffektTree(unit.module)
    nodes = positions.findNodesContaining(tree.nodes, position).sortWith {
      (t1, t2) =>
        val p1s = positions.getStart(t1).get
        val p2s = positions.getStart(t2).get

        if (p2s == p1s) {
          val p1e = positions.getFinish(t1).get
          val p2e = positions.getFinish(t2).get
          p1e < p2e
        } else {
          p2s < p1s
        }
    }
  } yield (nodes, unit)

  override def getDefinition(position: Position): Option[Tree] = for {
    (trees, unit) <- getInfoAt(position)
    id <- trees.collectFirst { case id: source.Id => id  }
    decl <- context.lookup(id) match {
      case u: UserFunction =>
        Some(u.decl)
      case u: Binder => Some(u.decl)
      case d: EffectOp => context.getDefinitionTree(d.effect)
      case u => context.getDefinitionTree(u)
    }
  } yield decl

  override def getHover(position : Position): Option[String] = for {
    (trees, unit) <- getInfoAt(position)

    (tree, tpe) <- trees.collectFirst {
      case id: Id if context.get(id).isDefined =>
        (id, context.get(id).get match {
          case b: BuiltinFunction => b.toType
          case s: ValueSymbol => context.valueType(s)
          case b: BlockSymbol => context.blockType(b)
          case t: TypeSymbol => t
          case other => sys error s"unknown symbol kind ${other}"
        })
      case e: Literal[t] if context.annotation(e).isDefined =>
        (e, context.annotation(e).get)
    }
  } yield tpe.toString

  // The implementation in kiama.Server does not support file sources
  override def locationOfNode(node : Tree) : Location = {
    (positions.getStart(node), positions.getFinish(node)) match {
      case (start @ Some(st), finish @ Some(_)) =>
        val s = convertPosition(start)
        val f = convertPosition(finish)
        new Location(st.source.name, new LSPRange(s, f))
      case _ =>
          null
    }
  }
}

object Server extends LSPServer