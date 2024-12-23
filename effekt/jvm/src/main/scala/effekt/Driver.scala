package effekt

// Adapted from
//   https://github.com/inkytonik/kiama/blob/master/extras/src/test/scala/org/bitbucket/inkytonik/kiama/example/oberon0/base/Driver.scala

import effekt.source.{ ModuleDecl, Tree }
import effekt.symbols.Module
import effekt.context.{ Context, IOModuleDB }
import kiama.output.PrettyPrinterTypes.Document
import kiama.parsing.ParseResult
import kiama.util.{ IO, Source }
import effekt.util.messages.{ BufferedMessaging, CompilerPanic, EffektError, EffektMessaging, FatalPhaseError, PlainTextError }
import effekt.util.paths.file
import effekt.util.{ AnsiColoredMessaging, MarkdownSource, getOrElseAborting }

import scala.sys.process.Process

/**
 * effekt.Compiler <----- compiles code with  ------ Driver ------ implements UI with -----> kiama.util.Compiler
 */
trait Driver extends kiama.util.Compiler[EffektConfig, EffektError] { outer =>

  object messaging extends AnsiColoredMessaging

  // Compiler context
  // ================
  // We always only have one global instance of the compiler
  object context extends Context(positions) with IOModuleDB { val messaging = outer.messaging }

  /**
   * If no file names are given, run the REPL
   */
  override def run(config: EffektConfig): Unit =
    if (config.repl()) {
      new Repl(this).run(config)
    // This is overridden by kiama.Server to launch the LSP server.
    // TODO: remove dynamic dispatch here and consider replacing inheritance by composition.
    } else if (config.server()) {
      super.run(config)
    } else for (filename <- config.filenames()) {
      compileFile(filename, config)
    }

  override def createConfig(args: Seq[String]) =
    new EffektConfig(args)

  /**
   * Main entry to the compiler, invoked by Kiama after creating the config
   *
   * In LSP mode: invoked for each file opened in an editor
   */
  override def compileSource(source: Source, config: EffektConfig): Unit = try {
    val src = if (source.name.endsWith(".md")) { MarkdownSource(source) } else { source }

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

        // we are in one of three exclusive modes: LSPServer, Compile, Run
        if (config.server()) { compiler.runFrontend(src) }
        else if (config.interpret()) { compile() foreach runner.eval }
        else if (config.build()) { compile() foreach runner.build }
        else if (config.compile()) { compile() }
    }
  } catch {
    case FatalPhaseError(msg) => context.report(msg)
    case e @ CompilerPanic(msg) => generateCrashReport(e, context, config, msg)

    // when in server-mode, do not crash but report the error to avoid
    // restarting the server.
    case e if config.server() =>
      context.info("Effekt Compiler Crash: " + e.getMessage)
      e.getStackTrace.foreach { line =>
        context.info("  at " + line)
      }
  } finally {
    outputTimes(source, config)(context)
    showIR(source, config)(context)
    writeIRs(source, config)(context)
    // This reports error messages
    afterCompilation(source, config)(context)
  }

  def generateCrashReport(e: Throwable, context: Context, config: EffektConfig, msg:effekt. util. messages. EffektError): Unit = {
    import java.nio.file.{Files, Paths}
    import java.nio.charset.StandardCharsets

    // Capture general information
    val errorMessage = msg match {
      case PlainTextError(content, _, _) => content
      case _ => msg
    }


    val effektVersion = effekt.util.Version.effektVersion
    val osInfoName = System.getProperty("os.name")
    val osInfoArch = System.getProperty("os.arch")
    val osInfoVersion = System.getProperty("os.version")

    val javaInfoVersion = System.getProperty("java.version")
    val javaInfoVendor = System.getProperty("java.vendor")
    val javaInfoRuntime = System.getProperty("java.runtime.name")

    val errorReport = "The compiler unexpectedly panicked. This is a compiler bug.\nPlease report it:"
    val issueLink = "https://github.com/effekt-lang/effekt/issues/new?labels=bug"
    val stringWriter = new java.io.StringWriter()
    e.printStackTrace(new java.io.PrintWriter(stringWriter))
    val stackTrace = stringWriter.toString

    // Collect backend details (if applicable)
    val backendInfoName = config.backend() match {
      case Backend(name, _, _) => name
      case null => "Backend-specific info Name not retrievable"
    }

    // Arguments passed to the compiler
    val argsMarkdown = config.args.mkString("\n", "\n", "\n")

    // Construct the report content
    val reportContent =
      s"""|# Effekt Compiler Crash Report
          |
          |**ERROR** : $errorMessage
          |
          |$errorReport
          |[Submit an issue]($issueLink)
          |
          |### Compiler Information
          |Effekt Version: $effektVersion \n
          |Backend: $backendInfoName
          |
          |### System Information
          |Operating System: $osInfoName \n Arch: $osInfoArch \n Version: $osInfoVersion
          |
          |JVM Version: $javaInfoVersion ($javaInfoVendor, $javaInfoRuntime)
          |
          |### Full Stack Trace
          |```
          |$stackTrace
          |```
          |
          |### Arguments Passed to the Compiler
          |```
          |$argsMarkdown
          |```
          |""".stripMargin

    // Write the report to a Markdown file
    val outputPath = Paths.get(config.outputPath().getAbsolutePath)
    val outputFile = outputPath.resolve("crash-report.md")
    try {
      Files.createDirectories(outputPath.getParent)
      Files.write(outputFile, reportContent.getBytes(StandardCharsets.UTF_8))
      //context.info(s"Crash report saved to: $outputPath")
    } catch {
      case ex: Exception =>
        context.info("Failed to save crash report to file: " + ex.getMessage)
    }

    // Provide the report link in the user-visible output
    val report =
      s"""|=== Effekt Compiler Crash Report ===
          |
          |[Error] $errorMessage
          |
          |The compiler unexpectedly panicked. This is a compiler bug.
          |Please report it: $issueLink
          |
          |Effekt Version: $effektVersion\n
          |Backend used: $backendInfoName
          |
          |A detailed crash report has been written to:
          |${outputPath.toAbsolutePath}
          |
          |Operating System: $osInfoName
          |
          |JVM Version: $javaInfoVersion
          |
          |Full Stack Trace:
          |$stackTrace
          |
          |Arguments passed to the compiler:
          |$argsMarkdown
          |""".stripMargin

    // Print the report to console
    context.info(report)
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
}
