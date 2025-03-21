package effekt

import java.util.concurrent.{CompletableFuture, ExecutorService, Executors}
import java.io.{InputStream, OutputStream, PrintWriter}
import java.net.ServerSocket
import java.nio.file.Paths
import java.util
import scala.jdk.FunctionConverters.*
import com.google.gson.JsonElement
import effekt.KiamaUtils.{convertRange, fromLSPPosition, locationOfNode}
import org.eclipse.lsp4j.jsonrpc.{Launcher, messages}
import org.eclipse.lsp4j.{Diagnostic, DiagnosticRelatedInformation, DiagnosticSeverity, DidChangeConfigurationParams, DidChangeTextDocumentParams, DidChangeWatchedFilesParams, DidCloseTextDocumentParams, DidOpenTextDocumentParams, DidSaveTextDocumentParams, DocumentSymbol, DocumentSymbolParams, Hover, HoverParams, InitializeParams, InitializeResult, Location, MarkupContent, PublishDiagnosticsParams, ReferenceParams, ServerCapabilities, SymbolInformation, TextDocumentIdentifier, TextDocumentSyncKind, WorkspaceFolder, Position as LSPPosition, Range as LSPRange}
import org.eclipse.lsp4j.services.{LanguageClient, LanguageClientAware, LanguageServer, TextDocumentService, WorkspaceService}
import org.eclipse.lsp4j.launch.LSPLauncher
import kiama.util.Collections.seqToJavaList
import kiama.util.Severities.{Error, Hint, Information, Severity, Warning}
import kiama.util.{Collections, Position, Source}
import effekt.Server.getSymbolKind
import effekt.context.Context
import effekt.source.Tree
import effekt.symbols.{Anon, Binder, UserFunction}
import effekt.util.PlainMessaging
import effekt.util.messages.EffektError
import effekt.symbols.isSynthetic

/**
 * Next generation LSP server for Effekt based on lsp4j directly instead of using Kiama
 */
class ServerNG(config: EffektConfig) extends LanguageServer with LanguageClientAware with Driver with Intelligence {
  private var client: LanguageClient = _
  private val textDocumentService = new EffektTextDocumentService(this)
  private val workspaceService = new EffektWorkspaceService

  // Track whether shutdown has been requested
  private var shutdownRequested: Boolean = false

  val getDriver: Driver = this
  val getConfig: EffektConfig = config

  object lspMessaging extends PlainMessaging

  // LSP Lifecycle
  //
  //

  override def initialize(params: InitializeParams): CompletableFuture[InitializeResult] = {
    val capabilities = new ServerCapabilities()
    capabilities.setTextDocumentSync(TextDocumentSyncKind.Full)
    capabilities.setHoverProvider(true)
    capabilities.setDefinitionProvider(true)
    capabilities.setReferencesProvider(true)
    capabilities.setDocumentSymbolProvider(true)
    capabilities.setCodeActionProvider(true)
    capabilities.setDocumentFormattingProvider(true)

    val result = new InitializeResult(capabilities)
    CompletableFuture.completedFuture(result)
  }

  override def shutdown(): CompletableFuture[Object] = {
    shutdownRequested = true
    CompletableFuture.completedFuture(null)
  }

  override def exit(): Unit = {
    System.exit(if (shutdownRequested) 0 else 1)
  }

  override def getTextDocumentService(): EffektTextDocumentService = textDocumentService

  override def getWorkspaceService(): EffektWorkspaceService = workspaceService

  // LSP Diagnostics
  //
  //

  def clearDiagnostics(name: String): Unit = {
    publishDiagnostics(name, Vector())
  }

  def publishDiagnostics(name: String, diagnostics: Vector[Diagnostic]): Unit = {
    val params = new PublishDiagnosticsParams(toURI(name), Collections.seqToJavaList(diagnostics))
    client.publishDiagnostics(params)
  }

  // Driver methods
  //
  //

  override def afterCompilation(source: Source, config: EffektConfig)(implicit C: Context): Unit = {
    val messages = C.messaging.buffer
    val groups = messages.groupBy(msg => msg.sourceName.getOrElse(""))
    for ((name, msgs) <- groups) {
      publishDiagnostics(name, msgs.distinct.map(KiamaUtils.messageToDiagnostic(lspMessaging)))
    }
  }

  // Other methods
  //
  //

  def toURI(filename: String): String = {
    if (filename.startsWith("file:") || filename.startsWith("vscode-notebook-cell:")) {
      // Already a URI or special scheme
      filename
    } else if (filename.startsWith("./") || filename.startsWith(".\\")) {
      // Remove the "./" or ".\\" prefix
      val relativePath = filename.substring(2)
      val cwd = System.getProperty("user.dir")
      val fullPath = Paths.get(cwd).resolve(relativePath).normalize()
      fullPath.toUri.toString
    } else {
      Paths.get(filename).toUri.toString
    }
  }

  override def connect(client: LanguageClient): Unit = {
    this.client = client
  }

  /**
   * Launch a language server using provided input/output streams.
   * This allows tests to connect via in-memory pipes.
   */
  def launch(client: LanguageClient, in: InputStream, out: OutputStream): Launcher[LanguageClient] = {
    val executor = Executors.newSingleThreadExecutor()
    val launcher =
      new LSPLauncher.Builder()
        .setLocalService(this)
        .setRemoteInterface(classOf[LanguageClient])
        .setInput(in)
        .setOutput(out)
        .setExecutorService(executor)
        .create()
    this.connect(client)
    launcher.startListening()
    launcher
  }

  /**
   * Launch a language server with a given `ServerConfig`
   */
  def launch(config: ServerConfig): Unit = {
    // Create a single-threaded executor to serialize all requests.
    val executor: ExecutorService = Executors.newSingleThreadExecutor()

    if (config.debug) {
      val serverSocket = new ServerSocket(config.debugPort)
      System.err.println(s"Starting language server in debug mode on port ${config.debugPort}")
      val socket = serverSocket.accept()

      val launcher =
        new LSPLauncher.Builder()
          .setLocalService(this)
          .setRemoteInterface(classOf[LanguageClient])
          .setInput(socket.getInputStream)
          .setOutput(socket.getOutputStream)
          .setExecutorService(executor)
          .traceMessages(new PrintWriter(System.err, true))
          .create()
      val client = launcher.getRemoteProxy
      this.connect(client)
      launcher.startListening()
    } else {
      val launcher =
        new LSPLauncher.Builder()
          .setLocalService(this)
          .setRemoteInterface(classOf[LanguageClient])
          .setInput(System.in)
          .setOutput(System.out)
          .setExecutorService(executor)
          .create()

      val client = launcher.getRemoteProxy
      this.connect(client)
      launcher.startListening()
    }
  }
}

class EffektTextDocumentService(server: ServerNG) extends TextDocumentService with Intelligence {
  // LSP Document Lifecycle
  //
  //

  def didChange(params: DidChangeTextDocumentParams): Unit = {
    val document = params.getTextDocument
    server.clearDiagnostics(document.getUri)
    server.getDriver.compileString(document.getUri, params.getContentChanges.get(0).getText, server.getConfig)
  }
  def didClose(params: DidCloseTextDocumentParams): Unit = {
    server.clearDiagnostics(params.getTextDocument.getUri)
  }
  def didOpen(params: DidOpenTextDocumentParams): Unit = {
    val document = params.getTextDocument
    server.clearDiagnostics(document.getUri)
    server.getDriver.compileString(document.getUri, document.getText, server.getConfig)
  }
  def didSave(params: DidSaveTextDocumentParams): Unit = {
    val document = params.getTextDocument
    server.clearDiagnostics(document.getUri)
    server.getDriver.compileString(document.getUri, params.getText, server.getConfig)
  }

  // LSP Hover
  //
  //

  override def hover(params: HoverParams): CompletableFuture[Hover] = {
    val position = server.sources.get(params.getTextDocument.getUri).map { source =>
      KiamaUtils.fromLSPPosition(params.getPosition, source)
    }
    position match
      case Some(position) => {
        val hover = getSymbolHover(position) orElse getHoleHover(position)
        val markup = new MarkupContent("markdown", hover.getOrElse(""))
        val result = new Hover(markup, new LSPRange(params.getPosition, params.getPosition))
        CompletableFuture.completedFuture(result)
      }
      case None => CompletableFuture.completedFuture(new Hover())
  }

  def getSymbolHover(position: Position): Option[String] = for {
    (tree, sym) <- getSymbolAt(position)(using server.context)
    info <- getInfoOf(sym)(using server.context)
  } yield if (this.server.getWorkspaceService().settingBool("showExplanations")) info.fullDescription else info.shortDescription

  def getHoleHover(position: Position): Option[String] = for {
    trees <- getTreesAt(position)(using server.context)
    tree <- trees.collectFirst { case h: source.Hole => h }
    info <- getHoleInfo(tree)(using server.context)
  } yield info

  // LSP Document Symbols
  //
  //

  override def documentSymbol(params: DocumentSymbolParams): CompletableFuture[util.List[messages.Either[SymbolInformation, DocumentSymbol]]] = {
    val source = server.sources.get(params.getTextDocument.getUri)
    if (source.isEmpty) return CompletableFuture.completedFuture(Collections.seqToJavaList(Vector()))

    server.context.compiler.runFrontend(source.get)(using server.context)

    val documentSymbols = for {
      sym <- server.context.sourceSymbolsFor(source.get).toVector
      if !sym.isSynthetic
      id <- server.context.definitionTreeOption(sym)
      decl <- getSourceTreeFor(sym)
      kind <- getSymbolKind(sym)
      detail <- getInfoOf(sym)(using server.context)
      declRange = convertRange(server.positions.getStart(decl), server.positions.getFinish(decl))
      idRange = convertRange(server.positions.getStart(id), server.positions.getFinish(id))
    } yield new DocumentSymbol(sym.name.name, kind, declRange, idRange, detail.header)

    val result = Collections.seqToJavaList(
      documentSymbols.map(sym => messages.Either.forRight[SymbolInformation, DocumentSymbol](sym))
    )
    CompletableFuture.completedFuture(result)
  }

  def getSourceTreeFor(sym: effekt.symbols.Symbol): Option[Tree] = sym match {
    case a: Anon => Some(a.decl)
    case f: UserFunction => Some(f.decl)
    case b: Binder => Some(b.decl)
    case _ => server.context.definitionTreeOption(sym)
  }

  // LSP References
  //
  //

  override def references(params: ReferenceParams): CompletableFuture[util.List[_ <: Location]] = {
    val position = server.sources.get(params.getTextDocument.getUri).map { source =>
      fromLSPPosition(params.getPosition, source)
    }
    if (position.isEmpty)
      return CompletableFuture.completedFuture(Collections.seqToJavaList(Vector()))

    val locations = for {
      (tree, sym) <- getSymbolAt(position.get)(using server.context)
      refs = server.context.distinctReferencesTo(sym)
      // getContext may be null!
      includeDeclaration = Option(params.getContext).exists(_.isIncludeDeclaration)
      allRefs = if (includeDeclaration) tree :: refs else refs
      locations = allRefs.map(ref => locationOfNode(server.positions, ref))
    } yield locations

    CompletableFuture.completedFuture(Collections.seqToJavaList(locations.getOrElse(Seq[Location]())))
  }
}

class EffektWorkspaceService extends WorkspaceService {
  var settings: JsonElement = null

  // LSP methods
  //
  //

  def didChangeConfiguration(params: DidChangeConfigurationParams): Unit = {
    this.settings = params.getSettings.asInstanceOf[JsonElement].getAsJsonObject
  }

  def didChangeWatchedFiles(params: DidChangeWatchedFilesParams): Unit = {}

  // Settings
  //
  //

  def settingBool(name: String): Boolean = {
    if (settings == null) return false
    val obj = settings.getAsJsonObject
    if (obj == null) return false
    val value = obj.get(name)
    if (value == null) return false
    value.getAsBoolean
  }
}

case class ServerConfig(debug: Boolean = false, debugPort: Int = 5000)
