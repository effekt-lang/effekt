package effekt

// Adapted from
//   https://bitbucket.org/inkytonik/kiama/src/master/extras/src/test/scala/org/bitbucket/inkytonik/kiama/example/oberon0/base/Driver.scala

import effekt.source.{ ModuleDecl, Tree }
import effekt.symbols.Module
import effekt.context.{ Context, IOModuleDB }
import effekt.util.{ ColoredMessaging, MarkdownSource }
import org.bitbucket.inkytonik.kiama
import kiama.output.PrettyPrinterTypes.Document
import kiama.parsing.ParseResult
import kiama.util.{ Client, CompilerWithConfig, IO, Services, Source }
import java.io.{ InputStream, OutputStream }

import effekt.util.messages.FatalPhaseError
import effekt.util.paths._
import org.eclipse.lsp4j.jsonrpc.Launcher

import scala.concurrent.ExecutionException
import scala.sys.process.Process

/**
 * Compiler <----- compiles code with  ------ Driver ------ implements UI with -----> kiama.CompilerWithConfig
 */
trait Driver extends CompilerWithConfig[Tree, ModuleDecl, EffektConfig] { outer =>

  val name = "effekt"

  override val messaging = new ColoredMessaging(positions)

  // Compiler context
  // ================
  // We always only have one global instance of the compiler
  object context extends Context(positions) with IOModuleDB {
    /**
     * Output: JavaScript -> File
     */
    override def saveOutput(doc: String, path: String)(implicit C: Context): Unit =
      if (C.config.requiresCompilation()) {
        C.config.outputPath().mkdirs
        IO.createFile(path, doc)
      }
  }

  /**
   * If no file names are given, run the REPL
   */
  override def run(config: EffektConfig): Unit =
    if (config.filenames().isEmpty && !config.server() && !config.compile()) {
      new Repl(this).run(config)
    } else {
      super.run(config)
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
      doc <- C.generate(src)
      if config.interpret()
      mod <- C.frontend(src)
    } eval(mod)

    afterCompilation(source, config)
  }

  def afterCompilation(source: Source, config: EffektConfig)(implicit C: Context): Unit = {
    // report messages
    report(source, C.buffer.get, config)
  }

  def eval(mod: Module)(implicit C: Context): Unit = C.at(mod.decl) {
    C.config.generator() match {
      case gen if gen.startsWith("js")   => evalJS(mod)
      case gen if gen.startsWith("chez") => evalCS(mod)
      case gen if gen.startsWith("llvm") => evalLLVM(mod)
    }
  }

  def evalJS(mod: Module)(implicit C: Context): Unit = C.at(mod.decl) {
    try {
      val _ = C.checkMain(mod)
      val jsFile = C.codeGenerator.path(mod)
      val jsScript = s"require('${jsFile}').main().run()"
      val command = Process(Seq("node", "--eval", jsScript))
      C.config.output().emit(command.!!)
    } catch {
      case FatalPhaseError(e) =>
        C.error(e)
    }
  }

  def evalCS(mod: Module)(implicit C: Context): Unit = C.at(mod.decl) {
    try {
      val _ = C.checkMain(mod)
      val csFile = C.codeGenerator.path(mod)
      val command = Process(Seq("scheme", "--script", csFile))
      C.config.output().emit(command.!!)
    } catch {
      case FatalPhaseError(e) =>
        C.error(e)
    }
  }

  def evalLLVM(mod: Module)(implicit C: Context): Unit = C.at(mod.decl) {
    try {
      C.checkMain(mod)
      val result: Document = C.generate(mod.source).get // TODO this might be `None`

      val xPath = (suffix: String) => (m: Module) => C.codeGenerator.path(m) + suffix
      val llvmPath = xPath(".ll")
      val optPath = xPath("_opt.ll")
      val objPath = xPath(".o")

      C.saveOutput(result.layout, llvmPath(mod))
      val optCommand = Process(Seq(s"opt-${C.LLVM_VERSION}", llvmPath(mod), "-S", "-O2", "-o", optPath(mod)))
      C.config.output().emit(optCommand.!!)

      val llcCommand = Process(Seq(s"llc-${C.LLVM_VERSION}", "--relocation-model=pic", optPath(mod), "-filetype=obj", "-o", objPath(mod)))
      C.config.output().emit(llcCommand.!!)

      val gccMainFile = (C.config.libPath / "main.c").unixPath
      val executableFile = C.codeGenerator.path(mod)
      val gccCommand = Process(Seq("gcc", gccMainFile, "-o", executableFile, objPath(mod)))
      C.config.output().emit(gccCommand.!!)

      val command = Process(Seq(executableFile))
      C.config.output().emit(command.!!)
    } catch {
      case FatalPhaseError(e) => C.error(e)
    }
  }

  def report(in: Source)(implicit C: Context): Unit =
    report(in, C.buffer.get, C.config)

  /**
   * Main entry to the compiler, invoked by Kiama after parsing with `parse`.
   * Not used anymore
   */
  override def process(source: Source, ast: ModuleDecl, config: EffektConfig): Unit = ???

  /**
   * Modified copy of kiama.ServerWithConfig.launch()
   *
   * When the --debug flag is used together with --server, we open the
   * server on port 5007 instead of stdin and out. This way a modified
   * vscode client can connect to the running server, aiding development
   * of the language server and clients.
   *
   * In a vscode extension, the vscode client can connect to the server using
   * the following example code:
   *
   *   let serverOptions = () => {
   *     // Connect to language server via socket
   *     let socket: any = net.connect({ port: 5007 });
   *     let result: StreamInfo = {
   *       writer: socket,
   *       reader: socket
   *     };
   *     return Promise.resolve(result);
   *   };
   *
   * @see https://github.com/microsoft/language-server-protocol/issues/160
   */
  override def launch(config: EffektConfig): Unit = {
    if (config.debug()) {
      import java.net.InetSocketAddress
      import java.nio.channels.{ AsynchronousServerSocketChannel, Channels }

      val port = 5007
      val addr = new InetSocketAddress("localhost", port)
      val socket = AsynchronousServerSocketChannel.open().bind(addr);

      try {
        println(s"Waiting on port ${port} for LSP clients to connect")
        val ch = socket.accept().get();
        println(s"Connected to LSP client")
        val in = Channels.newInputStream(ch)
        val out = Channels.newOutputStream(ch)
        launch(config, in, out)
      } catch {
        case e: InterruptedException =>
          e.printStackTrace()
        case e: ExecutionException =>
          e.printStackTrace()
      } finally {
        socket.close()
      }
    } else {
      launch(config, System.in, System.out)
    }
  }

  private def launch(config: EffektConfig, in: InputStream, out: OutputStream): Unit = {
    val services = new Services(this, config)
    val launcherBase =
      new Launcher.Builder[Client]()
        .setLocalService(services)
        .setRemoteInterface(classOf[Client])
        .setInput(in)
        .setOutput(out)
    val launcher = launcherBase.create()
    val client = launcher.getRemoteProxy()
    connect(client)
    launcher.startListening()
  }

  /**
   * Originally called by kiama, not used anymore.
   */
  override def parse(source: Source): ParseResult[ModuleDecl] = ???

  def format(m: ModuleDecl): Document = ???
}
