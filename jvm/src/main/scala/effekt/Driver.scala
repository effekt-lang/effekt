package effekt

// Adapted from
//   https://bitbucket.org/inkytonik/kiama/src/master/extras/src/test/scala/org/bitbucket/inkytonik/kiama/example/oberon0/base/Driver.scala

import effekt.source.{ ModuleDecl, Tree }
import effekt.symbols.{ Module, builtins }
import effekt.context.{ Context, IOModuleDB }

import org.bitbucket.inkytonik.kiama
import kiama.output.PrettyPrinterTypes.Document
import kiama.parsing.ParseResult
import kiama.util.{ Source, REPLConfig, CompilerWithConfig, IO }

import org.rogach.scallop._

import java.io.File


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


trait Driver extends Compiler with CompilerWithConfig[Tree, ModuleDecl, EffektConfig] { driver =>

  import effekt.evaluator.Evaluator

  val name = "effekt"

  object evaluator extends Evaluator

  // Compiler context
  // ================
  // We always only have one global instance of CompilerContext
  object context extends Context with IOModuleDB {

    /**
     * This is used to resolve imports
     */
    def process(source: Source): Module =
      compile(source)(this) getOrElse {
        abort(s"Error processing dependency: ${source.name}")
      }

    /**
     * This is used by the LSP Server to perform type checking.
     */
    def frontend(source: Source): Option[Module] =
      driver.frontend(source)(this)

    populate(builtins.rootTerms.values)
  }

  /**
   * If no file names are given, run the REPL
   */
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

    for {
      mod <- pipeline(source, ast)
      if !config.server() && !config.compile()
    } evaluator.run(mod)

    // report messages
    clearSyntacticMessages(source, config)
    clearSemanticMessages(source, config)
    report(source, C.buffer.get, config)
  }


  /**
   * Output: JavaScript -> File
   */
  override def saveOutput(js: Document, unit: Module)(implicit C: Context): Unit =
    if (C.config.compile()) {
      val outDir = C.config.outputPath().toPath
      outDir.toFile.mkdirs
      val out = outDir.resolve(unit.outputName).toFile

      println("Writing compiled Javascript to " + out)
      IO.createFile(out.getCanonicalPath, js.layout)
    }

  def report(in: Source)(implicit C: Context): Unit =
    report(in, C.buffer.get, C.config)

  def format(m: ModuleDecl) : Document = ???
}
