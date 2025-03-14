package effekt

import java.util.concurrent.CompletableFuture
import java.util.{List => JList}
import java.io.{InputStream, OutputStream}
import java.net.ServerSocket

import org.eclipse.lsp4j.{
  InitializeParams, InitializeResult, ServerCapabilities,
  TextDocumentSyncKind, WorkspaceFolder
}
import org.eclipse.lsp4j.services.{
  LanguageClient, LanguageClientAware, LanguageServer,
  TextDocumentService, WorkspaceService
}
import org.eclipse.lsp4j.launch.LSPLauncher
import org.eclipse.lsp4j.DidChangeTextDocumentParams
import org.eclipse.lsp4j.DidCloseTextDocumentParams
import org.eclipse.lsp4j.DidOpenTextDocumentParams
import org.eclipse.lsp4j.DidSaveTextDocumentParams
import org.eclipse.lsp4j.DidChangeConfigurationParams
import org.eclipse.lsp4j.DidChangeWatchedFilesParams

/**
 * Next generation LSP server for Effekt based on lsp4j directly instead of using Kiama
 */
class ServerNG extends LanguageServer with LanguageClientAware {
  private var client: LanguageClient = _
  private val textDocumentService = new EffektTextDocumentService
  private val workspaceService = new EffektWorkspaceService

  override def initialize(params: InitializeParams): CompletableFuture[InitializeResult] = {
    val capabilities = new ServerCapabilities()
    capabilities.setTextDocumentSync(TextDocumentSyncKind.Full)

    val result = new InitializeResult(capabilities)
    CompletableFuture.completedFuture(result)
  }

  override def shutdown(): CompletableFuture[Object] = {
    CompletableFuture.completedFuture(null)
  }

  override def exit(): Unit = {}

  override def getTextDocumentService(): TextDocumentService = textDocumentService

  override def getWorkspaceService(): WorkspaceService = workspaceService

  override def connect(client: LanguageClient): Unit = {
    this.client = client
  }
}

class EffektTextDocumentService extends TextDocumentService {
  def didChange(params: DidChangeTextDocumentParams): Unit = {}
  def didClose(params: DidCloseTextDocumentParams): Unit = {}
  def didOpen(params: DidOpenTextDocumentParams): Unit = {}
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
    if (config.debug) {
      val serverSocket = new ServerSocket(config.debugPort)
      println(s"Starting language server in debug mode on port ${config.debugPort}")
      val socket = serverSocket.accept()
      val launcher = LSPLauncher.createServerLauncher(
        server,
        socket.getInputStream,
        socket.getOutputStream
      )
      val client = launcher.getRemoteProxy
      server.connect(client)
      launcher.startListening()
    } else {
      val launcher = LSPLauncher.createServerLauncher(
        server,
        System.in,
        System.out
      )
      val client = launcher.getRemoteProxy
      server.connect(client)
      launcher.startListening()
    }
  }
}
