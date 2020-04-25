package effekt

// Adapted from
//   https://bitbucket.org/inkytonik/kiama/src/master/extras/src/test/scala/org/bitbucket/inkytonik/kiama/example/oberon0/base/Driver.scala

import effekt.source.{ ModuleDecl, Tree }
import effekt.symbols.Module
import effekt.context.{ Context, IOModuleDB }
import effekt.util.{ ColoredMessaging, MarkdownSource }
import effekt.util.paths._
import org.bitbucket.inkytonik.kiama
import kiama.output.PrettyPrinterTypes.Document
import kiama.parsing.ParseResult
import kiama.util.{ CompilerWithConfig, IO, Source }
import java.io.{ File => JFile }

import scala.sys.process.Process

trait Driver extends Compiler with CompilerWithConfig[Tree, ModuleDecl, EffektConfig] { outer =>

  val name = "effekt"

  override val messaging = new ColoredMessaging(positions)

  // Compiler context
  // ================
  // We always only have one global instance of CompilerContext
  object context extends Context(outer) with IOModuleDB

  /**
   * If no file names are given, run the REPL
   */
  override def run(config: EffektConfig): Unit = {

    config._libPath = Some(findStdLib(config).toFile)

    if (config.filenames().isEmpty && !config.server()) {
      new Repl(this).run(config)
    } else {
      super.run(config)
    }
  }

  override def createConfig(args: Seq[String]) =
    new EffektConfig(args)

  /**
   * Main entry to the compiler, invoked by Kiama after creating the config
   *
   * In LSP mode: invoked for each file opened in an editor
   */
  override def compileSource(source: Source, config: EffektConfig): Unit = {
    val src = if (source.name.endsWith(".md")) { MarkdownSource(source) } else { source }

    sources(source.name) = src

    implicit val C = context
    C.setup(config)

    for {
      _ <- compile(src)
      if config.interpret()
      mod <- frontend(src)
    } eval(mod)

    afterCompilation(source, config)
  }

  def afterCompilation(source: Source, config: EffektConfig)(implicit C: Context): Unit = {
    // report messages
    report(source, C.buffer.get, config)
  }

  /**
   * Output: JavaScript -> File
   */
  override def saveOutput(js: Document, unit: Module)(implicit C: Context): Unit =
    if (C.config.requiresCompilation()) {
      C.config.outputPath().mkdirs
      IO.createFile(jsPath(unit), js.layout)
    }

  def eval(mod: Module)(implicit C: Context): Unit = C.at(mod.decl) {

    val mains = mod.terms.getOrElse("main", Set())

    if (mains.isEmpty) {
      C.error("No main function defined")
      return
    }

    if (mains.size > 1) {
      C.error("Multiple main functions defined")
      return
    }

    val main = mains.head

    val mainParams = C.blockTypeOf(main).params
    if ((mainParams.size != 1) || (mainParams.head != Nil)) {
      C.error("Main does not take arguments")
      return
    }

    val tpe = C.blockTypeOf(main)
    val userEffects = tpe.ret.effects.userDefined
    if (userEffects.nonEmpty) {
      C.error(s"Main cannot have user defined effects, but includes effects: ${userEffects}")
      return
    }

    val jsFile = jsPath(mod)
    val jsScript = s"require('${jsFile}').main().run()"
    val command = Process(Seq("node", "--eval", jsScript))
    C.config.output().emit(command.!!)
  }

  /**
   * JavaScript paths are *not* platform dependent.
   */
  def jsPath(mod: Module)(implicit C: Context): String =
    (C.config.outputPath() / moduleFileName(mod.path)).unixPath

  def report(in: Source)(implicit C: Context): Unit =
    report(in, C.buffer.get, C.config)

  /**
   * Main entry to the compiler, invoked by Kiama after parsing with `parse`.
   * Not used anymore
   */
  override def process(source: Source, ast: ModuleDecl, config: EffektConfig): Unit = ???

  /**
   * Originally called by kiama, not used anymore.
   */
  override def parse(source: Source): ParseResult[ModuleDecl] = ???

  /**
   * Tries to find the path to the standard library. Proceeds in the following
   * order:
   * 1) specified as part of the settings arg `lib`?
   * 2) specified in an environment variable `EFFEKT_LIB`
   * 3) relative to the current working directory
   * 4) relative to to the executed JAR file (effekt.jar)
   */
  def findStdLib(config: EffektConfig): File = {

    // 1) in config?
    config.stdlibPath.foreach { path =>
      return path
    }

    // 2) in PATH
    if (System.getenv.containsKey("EFFEKT_LIB")) {
      return System.getenv("EFFEKT_LIB")
    }

    // 3) in PWD
    val pwd = file(".")
    if ((pwd / "lib" / "effekt.effekt").exists) {
      return pwd / "lib"
    }

    // 4) next to Jar
    val jarPath = try {
      file(getClass.getProtectionDomain.getCodeSource.getLocation.toURI).parent
    } catch {
      case e: Throwable =>
        sys.error("Cannot find path to standard library")
    }

    if ((jarPath / ".." / "lib" / "effekt.effekt").exists) {
      return jarPath / ".." / "lib"
    }

    sys.error("Cannot find path to standard library")
  }

  /**
   * Creates a temporary folder and copies all .effekt and .js files into this folder
   *
   * This is how we establish that the files are present for the IDE to jump to definition.
   *
   * It registers all temporary files and folders for deletion, once the JVM shuts down gracefully.
   */
  @deprecated
  def extractJarFile(): JFile = {
    import java.nio.file.{ FileSystems, Files, Paths, Path }

    // register for automatic deletion
    def deleteLater(p: Path): Unit =
      p.toFile.deleteOnExit()

    val tmpFolder = Files.createTempDirectory("effekt_lib")
    deleteLater { tmpFolder }

    // get the class loader and try to find a single resource to have obtain a valid path
    val cl = Thread.currentThread().getContextClassLoader()
    val url = cl.getResource("effekt.effekt")
    val fs = FileSystems.newFileSystem(url.toURI, new java.util.HashMap[String, Any]())

    Files.walk(fs.getPath(".")).forEach { source =>
      val sourceName = source.toString
      val target = tmpFolder.resolve(sourceName)

      // only copy effekt and js files
      if (sourceName.endsWith(".effekt") || sourceName.endsWith(".js")) deleteLater {
        if (source.getParent != null) deleteLater {
          Files.createDirectories(tmpFolder.resolve(source.getParent.toString))
        }
        Files.copy(source, target)
      }
    }
    tmpFolder.toFile
  }

  def format(m: ModuleDecl): Document = ???
}
