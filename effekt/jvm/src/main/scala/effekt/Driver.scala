package effekt

// Adapted from
//   https://github.com/inkytonik/kiama/blob/master/extras/src/test/scala/org/bitbucket/inkytonik/kiama/example/oberon0/base/Driver.scala

import effekt.context.{Context, IOModuleDB}
import effekt.source.ModuleDecl
import effekt.util.messages.{CompilerPanic, FatalPhaseError}
import effekt.util.paths.file
import effekt.util.{AnsiColoredMessaging, JSONDocumentationGenerator, MarkdownSource}
import kiama.output.PrettyPrinterTypes.Document
import kiama.util.{FileSource, IO, Source, StringSource}

import scala.collection.mutable

/**
 * effekt.Compiler <----- compiles code with  ------ Driver
 */
trait Driver { outer =>

  object messaging extends AnsiColoredMessaging

  val sources = mutable.Map[String, Source]()

  // Compiler context
  // ================
  // We always only have one global instance of the compiler
  object context extends Context with IOModuleDB { val messaging = outer.messaging }

  def createConfig(args: Seq[String]) =
    new EffektConfig(args)

  /**
   * Main entry to the compiler, invoked by Kiama after creating the config
   *
   * In LSP mode: invoked for each file opened in an editor
   */
  def compileSource(source: Source, config: EffektConfig): Unit = {
      val src = if (source.name.endsWith(".md")) { MarkdownSource(source) } else { source }
      try {
        // remember that we have seen this source, this is used by LSP (kiama.util.Server)
        sources(source.name) = src

        implicit val C = context
        C.setup(config)

        def saveOutput(path: String, doc: String): Unit =
          if (C.config.requiresCompilation()) {
            val out = C.config.outputPath()
            out.mkdirs
            IO.createFile((out / path).unixPath, doc)
          }

        C.backend match {

          case Backend(name, compiler, runner) =>
            // measure the total compilation time here
            def compile() = C.timed("total", source.name) {
              compiler.compile(src) map {
                case (outputFiles, exec) =>
                  outputFiles.foreach {
                    case (filename, doc) =>
                      saveOutput(filename, doc)
                  }
                  exec
              }
            }

            // we are in one of four exclusive modes: Documenter, LSPServer, Compile, Run
            if (config.documenter()) { documenter(source, config)(context) }
            else if (config.server()) { compiler.runFrontend(src) }
            else if (config.interpret()) { compile() foreach runner.eval }
            else if (config.build()) { compile() foreach runner.build }
            else if (config.compile()) { compile() }
        }
      } catch {
        case FatalPhaseError(msg) => context.report(msg)
        case e @ CompilerPanic(msg) =>
          context.report(msg)
          e.getStackTrace.foreach { line =>
            context.info("  at " + line)
          }
        // when in server-mode, do not crash but report the error to avoid
        // restarting the server.
        case e if config.server() =>
          context.info("Effekt Compiler Crash: " + e.getMessage)
          e.getStackTrace.foreach { line =>
            context.info("  at " + line)
          }
      } finally {
        outputTimes(src, config)(context)
        showIR(src, config)(context)
        writeIRs(src, config)(context)
        // This reports error messages
        afterCompilation(src, config)(context)
      }
  }

  /**
   * Outputs the timing information captured in [[effekt.util.Timers]] by [[effekt.context.Context]]. Either a JSON file
   * is written to disk or a plain text message is written to stdout.
   */
  def outputTimes(source: Source, config: EffektConfig)(implicit C: Context): Unit = {
    if (C.timersActive) config.time.toOption foreach {
      case "json" =>
        // extract source filename and write to given output path
        val out = config.outputPath().getAbsolutePath
        val name = s"${source.name.split("/").last.stripSuffix(".effekt")}.json"
        IO.createFile((out / name).unixPath, C.timesToJSON())
      case "text" =>
        C.info(C.timesToString())
    }
  }

  def showIR(source: Source, config: EffektConfig)(implicit C: Context): Unit =
    config.showIR().map { stage =>
      C.compiler.prettyIR(source, stage).map { case (Document(s, _)) => println(s) }
    }

  def writeIRs(source: Source, config: EffektConfig)(implicit C: Context): Unit = {
    if (!config.writeIRs()) return
    val out = config.outputPath().getAbsolutePath
    for (stage <- Stage.values)
      C.compiler.prettyIR(source, stage).map { case (Document(s, _)) =>
        val extension = if (stage == Stage.Target) C.runner.extension else "ir"
        val name = source.name.split("/").last + "-" + stage.toString.toLowerCase + "." + extension
        IO.createFile((out / name).unixPath, s)
      }
  }

  def generateDocumentation(ast: ModuleDecl, source: Source)(using C: Context): String =
    JSONDocumentationGenerator(ast, source.name).content

  def showDocumentation(ast: ModuleDecl, source: Source)(using C: Context): Unit =
    println(generateDocumentation(ast, source))

  def writeDocumentation(ast: ModuleDecl, source: Source, output: String)(using C: Context): Unit =
    val name = source.name.split("/").last + ".json"
    IO.createFile((output / name).unixPath, generateDocumentation(ast, source))

  def documenter(source: Source, config: EffektConfig)(implicit C: Context): Unit =
    C.compiler.runFrontend(source)
    val astOpt = C.compiler.getAST(source)
    if (astOpt.isEmpty) return

    val out = config.outputPath().getAbsolutePath
    if (config.writeDocumentation()) writeDocumentation(astOpt.get, source, out)
    if (config.showDocumentation()) showDocumentation(astOpt.get, source)

  /**
   * Overridden in [[Server]] to also publish core and js compilation results to VSCode
   */
  def afterCompilation(source: Source, config: EffektConfig)(implicit C: Context): Unit = {
    // report messages
    report(source, C.messaging.buffer, config)

    // exit with non-zero code if not in repl/server mode and messaging buffer contains errors
    if (config.exitOnError() && C.messaging.hasErrors)
      sys.exit(1)
  }

  def report(source: Source, messages: messaging.Messages, config: EffektConfig): Unit = {
    messaging.report(source, messages, config.output())
  }

  def compileString(name: String, input: String, config: EffektConfig): Unit = {
    compileSource(StringSource(input, name), config)
  }

  def compileFile(filename: String, config: EffektConfig, encoding: String = "UTF-8"): Unit = {
    try {
      compileSource(FileSource(filename, encoding), config)
    } catch {
      case e: java.io.FileNotFoundException =>
        config.output().emitln(e.getMessage)
    }
  }
}
