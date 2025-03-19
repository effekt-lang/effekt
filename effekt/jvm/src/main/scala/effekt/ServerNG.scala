package effekt

import kiama.util.Collections
import org.eclipse.lsp4j.jsonrpc.MessageConsumer

import java.util.concurrent.{CompletableFuture, ExecutorService, Executors}
import java.util.List as JList
import java.io.{InputStream, OutputStream}
import java.net.ServerSocket
import org.eclipse.lsp4j.{Diagnostic, DidChangeConfigurationParams, DidChangeTextDocumentParams, DidChangeWatchedFilesParams, DidCloseTextDocumentParams, DidOpenTextDocumentParams, DidSaveTextDocumentParams, InitializeParams, InitializeResult, PublishDiagnosticsParams, ServerCapabilities, TextDocumentSyncKind, WorkspaceFolder}
import org.eclipse.lsp4j.services.{LanguageClient, LanguageClientAware, LanguageServer, TextDocumentService, WorkspaceService}
import org.eclipse.lsp4j.launch.LSPLauncher
import scala.jdk.FunctionConverters._

import java.nio.file.Paths

/**
 * Next generation LSP server for Effekt based on lsp4j directly instead of using Kiama
 */
class ServerNG extends LanguageServer with LanguageClientAware {
  private var client: LanguageClient = _
  private val textDocumentService = new EffektTextDocumentService(this)
  private val workspaceService = new EffektWorkspaceService

  // Track whether shutdown has been requested
  private var shutdownRequested: Boolean = false

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

  override def getTextDocumentService(): TextDocumentService = textDocumentService

  override def getWorkspaceService(): WorkspaceService = workspaceService

  def clearDiagnostics(name: String): Unit = {
    publishDiagnostics(name, Vector())
  }

  def publishDiagnostics(name: String, diagnostics: Vector[Diagnostic]): Unit = {
    val params = new PublishDiagnosticsParams(toURI(name), Collections.seqToJavaList(diagnostics))
    client.publishDiagnostics(params)
  }

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
}

class EffektTextDocumentService(server: ServerNG) extends TextDocumentService {
  def didChange(params: DidChangeTextDocumentParams): Unit = {}
  def didClose(params: DidCloseTextDocumentParams): Unit = {}
  def didOpen(params: DidOpenTextDocumentParams): Unit = {
      val document = params.getTextDocument
      server.clearDiagnostics(document.getUri)
  }
  def didSave(params: DidSaveTextDocumentParams): Unit = {}
}

class EffektWorkspaceService extends WorkspaceService {
  def didChangeConfiguration(params: DidChangeConfigurationParams): Unit = {}
  def didChangeWatchedFiles(params: DidChangeWatchedFilesParams): Unit = {}
}

case class ServerConfig(debug: Boolean = false, debugPort: Int = 5000)

object ServerNG {
  /*
   * Launch a language server with a given `ServerConfig`
   */
  def launch(config: ServerConfig): Unit = {
    val server = new ServerNG()

    // Create a single-threaded executor to serialize all requests.
    val executor: ExecutorService = Executors.newSingleThreadExecutor()
    // Define an identity wrapper that returns the provided MessageConsumer unchanged.
    val wrapper = (consumer: MessageConsumer) => consumer

    if (config.debug) {
      val serverSocket = new ServerSocket(config.debugPort)
      println(s"Starting language server in debug mode on port ${config.debugPort}")
      val socket = serverSocket.accept()
      val launcher = LSPLauncher.createServerLauncher(
        server,
        socket.getInputStream,
        socket.getOutputStream,
        executor,
        wrapper.asJava
      )
      val client = launcher.getRemoteProxy
      server.connect(client)
      launcher.startListening()
    } else {
      val launcher = LSPLauncher.createServerLauncher(
        server,
        System.in,
        System.out,
        executor,
        wrapper.asJava
      )
      val client = launcher.getRemoteProxy
      server.connect(client)
      launcher.startListening()
    }
  }
}
