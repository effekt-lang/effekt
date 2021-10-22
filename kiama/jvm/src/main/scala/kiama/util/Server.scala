/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2018-2021 Anthony M Sloane, Macquarie University.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package kiama
package util

import kiama.util.Collections.{ mapToJavaMap, seqToJavaList }
import org.eclipse.lsp4j.{ Position => LSPPosition, Range => LSPRange, _ }

/**
 * A language server that is mixed with a compiler that provide the basis
 * for its services. Allows specialisation of configuration via `C`.
 */
trait Server[N, T <: N, C <: Config] extends Compiler[N, T, C] with LanguageService[N] {

  import com.google.gson.{ JsonArray, JsonElement, JsonObject }
  import java.util.Collections
  import java.util.concurrent.CompletableFuture
  import java.io.{ InputStream, OutputStream }
  import scala.concurrent.ExecutionException
  import output.PrettyPrinterTypes.{ Document, emptyDocument, LinkRange, LinkValue }
  import kiama.util.Messaging.Messages
  import kiama.util.Severities._
  import org.eclipse.lsp4j.jsonrpc.Launcher

  // Overriding endpoints to enable server functionality

  override def run(config: C): Unit =
    if (config.server())
      launch(config)
    else
      super.run(config)

  override def report(source: Source, messages: Messages, config: C): Unit =
    if (config.server())
      publishMessages(messages)
    else
      super.report(source, messages, config)

  override def clearSyntacticMessages(source: Source, config: C): Unit =
    if (config.server()) {
      publishSourceProduct(source)
      publishSourceTreeProduct(source)
    }

  // Monto support

  override def publishSourceProduct(source: Source, document: => Document = emptyDocument): Unit = {
    if (settingBool("showSource"))
      publishProduct(source, "source", name, document)
  }

  override def publishSourceTreeProduct(source: Source, document: => Document = emptyDocument): Unit = {
    if (settingBool("showSourceTree"))
      publishProduct(source, "sourcetree", "scala", document)
  }

  // Client saving

  private[this] var client: Client = _

  def connect(aClient: Client): Unit = {
    client = aClient
  }

  // Client settings saving

  private[this] var _settings: JsonObject = _

  def settings(): JsonObject =
    _settings

  def setSettings(settings: JsonObject): Unit = {
    _settings = settings
  }

  def setSettings(settings: Object): Unit = {
    setSettings(settings.asInstanceOf[JsonElement].getAsJsonObject)
  }

  def setting[V](key: String, get: JsonElement => V, default: V): V =
    if (settings() == null)
      default
    else {
      val value = settings().get(key)
      if (value == null)
        default
      else
        get(value)
    }

  def settingBool(key: String, default: Boolean = false): Boolean =
    setting(key, _.getAsBoolean, default)

  def settingInt(key: String, default: Int = 0): Int =
    setting(key, _.getAsInt, default)

  def settingStr(key: String, default: String = ""): String =
    setting(key, _.getAsString, default)

  def settingArray(key: String, default: JsonArray = new JsonArray()): JsonArray =
    setting(key, _.getAsJsonArray, default)

  // Launching

  /**
   * When the --debug flag is used together with --server, we open the
   * server on port 5007 (or on --debugPort) instead of stdin and out. This way a modified
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
  def launch(config: C): Unit = {
    if (config.debug()) {
      import java.net.InetSocketAddress
      import java.nio.channels.{ AsynchronousServerSocketChannel, Channels }

      val port = config.debugPort()
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

  def launch(config: C, in: InputStream, out: OutputStream): Unit = {
    val services = createServices(config)
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

  def createServices(config: C): Services[N, T, C] = new Services(this, config)

  // User messages

  def showMessage(tipe: MessageType, msg: String): Unit = {
    client.showMessage(new MessageParams(tipe, msg))
  }

  def logMessage(msg: String): Unit = {
    client.logMessage(new MessageParams(MessageType.Log, msg))
  }

  // Dynamic capabilities

  def registerCapability(id: String, method: String, options: Object): Unit = {
    val registration = new Registration(id, method, options)
    val params = new RegistrationParams(Collections.singletonList(registration))
    client.registerCapability(params)
  }

  // Diagnostics

  def publishMessages(messages: Messages): Unit = {
    val groups = messages.groupBy(messaging.name(_).getOrElse(""))
    for ((name, msgs) <- groups) {
      publishDiagnostics(name, msgs.map(messageToDiagnostic))
    }
  }

  def publishDiagnostics(name: String, diagnostics: Vector[Diagnostic]): Unit = {
    val uri = if (name startsWith "file://") name else s"file://$name"
    val params = new PublishDiagnosticsParams(uri, seqToJavaList(diagnostics))
    client.publishDiagnostics(params)
  }

  def clearDiagnostics(name: String): Unit = {
    publishDiagnostics(name, Vector())
  }

  def messageToDiagnostic(message: Message): Diagnostic = {
    val s = convertPosition(messaging.start(message))
    val f = convertPosition(messaging.finish(message))
    val range = new LSPRange(s, f)
    val severity = convertSeverity(message.severity)
    new Diagnostic(range, message.label, severity, name)
  }

  def convertPosition(optPos: Option[Position]): LSPPosition =
    optPos match {
      case Some(p) => new LSPPosition(p.line - 1, p.column - 1)
      case None    => new LSPPosition(0, 0)
    }

  def convertRange(optStart: Option[Position], optFinish: Option[Position]): LSPRange =
    new LSPRange(convertPosition(optStart), convertPosition(optFinish))

  def convertSeverity(severity: Severity): DiagnosticSeverity =
    severity match {
      case Error       => DiagnosticSeverity.Error
      case Warning     => DiagnosticSeverity.Warning
      case Information => DiagnosticSeverity.Information
      case Hint        => DiagnosticSeverity.Hint
    }

  // Monto

  // Monto is a service to send additional documents to the language client (like vscode). Depending on the client,
  // they can be rendered in additional window panes. This features is mostly used to show intermediate representations
  // and compilation output.
  // The additional documents can be linked to the original source file, allowing users making connections between
  // generated code and source code.

  case class RangePair(
    sstart: Int, send: Int,
    tstart: Int, tend: Int
  )

  def publishProduct(
    source: Source, name: String, language: String,
    document: Document = emptyDocument,
    append: Boolean = false
  ): Unit = {
    val uri = s"file://${source.name}"
    val content = document.layout
    val pairs = positionsOfDocument(document)
    val rangeMap = sortBySourceRangeSize(pairsToMap(pairs, pairToSourceRange, pairToTargetRange))
    val rangeMapRev = sortBySourceRangeSize(pairsToMap(pairs, pairToTargetRange, pairToSourceRange))
    client.publishProduct(
      Product(uri, name, language, content, append, rangeMap, rangeMapRev)
    )
  }

  def publishProductStr(
    source: Source, name: String, language: String,
    message: String = "", append: Boolean = false
  ): Unit = {
    publishProduct(source, name, language, Document(message, Nil), append)
  }

  def sortBySourceRangeSize(pairs: Array[RangeEntry]): Array[RangeEntry] =
    pairs.sortBy {
      entry => entry.source.end - entry.source.start
    }

  def pairsToMap(
    pairs: List[RangePair],
    key: RangePair => OffsetRange,
    value: RangePair => OffsetRange
  ): Array[RangeEntry] = {
    pairs.groupBy(key).toArray.map {
      case (s, ts) =>
        RangeEntry(s, ts.map(value).toArray)
    }
  }

  def pairToSourceRange(pair: RangePair): OffsetRange =
    OffsetRange(pair.sstart, pair.send)

  def pairToTargetRange(pair: RangePair): OffsetRange =
    OffsetRange(pair.tstart, pair.tend)

  def positionsOfDocument(document: Document): List[RangePair] =
    document.links.flatMap {
      case LinkValue(n, r) =>
        val start = positions.getStart(n)
        val finish = positions.getFinish(n)
        positionOfStartFinish(start, finish) match {
          case Some((s, f)) =>
            Some(RangePair(s, f, r.start, r.end))
          case None =>
            None
        }
      case LinkRange(f, t) =>
        Some(RangePair(f.start, f.end, t.start, t.end))
    }

  def positionOfStartFinish(optStart: Option[Position], optFinish: Option[Position]): Option[(Int, Int)] =
    (optStart, optFinish) match {
      case (Some(start), Some(finish)) =>
        (start.optOffset, finish.optOffset) match {
          case (Some(s), Some(f)) =>
            Some((s, f))
          case _ =>
            None
        }
      case _ =>
        None
    }

  // Support for services

  def locationOfNode(node: N): Location = {
    (positions.getStart(node), positions.getFinish(node)) match {
      case (start @ Some(st), finish @ Some(_)) =>
        st.source match {
          case StringSource(_, name) =>
            val s = convertPosition(start)
            val f = convertPosition(finish)
            new Location(name, new LSPRange(s, f))
          case _ =>
            null
        }
      case _ =>
        null
    }
  }

  def rangeOfNode(node: N): LSPRange =
    convertRange(positions.getStart(node), positions.getFinish(node))

}

trait LanguageService[N] {

  /**
   * A representation of a simple named code action that replaces
   * a tree node with other text.
   */
  // FIXME: can the "to" be a node too? But server can't access correct PP...
  case class TreeAction(name: String, uri: String, from: N, to: String)

  /**
   * Return applicable code actions for the given position (if any).
   * Each action is in terms of an old tree node and a new node that
   * replaces it. Default is to return no actions.
   */
  def getCodeActions(position: Position): Option[Vector[TreeAction]] =
    None

  /**
   * Return the corresponding definition node for the given position
   * (if any). Default is to never return anything.
   */
  def getDefinition(position: Position): Option[N] =
    None

  /**
   * Return a formatted version of the whole of the given source.
   * By default, return `None` meaning there is no formatter.
   */
  def getFormatted(source: Source): Option[String] =
    None

  /**
   * Return markdown hover markup for the given position (if any).
   * Default is to never return anything.
   */
  def getHover(position: Position): Option[String] =
    None

  /**
   * Return the corresponding reference nodes (uses) of the symbol
   * at the given position (if any). If `includeDecl` is true, also
   * include the declaration of the symbol. Default is to never return
   * anything.
   */
  def getReferences(position: Position, includeDecl: Boolean): Option[Vector[N]] =
    None

  /**
   * Return the symbols frmo a compilation unit. Default is to return
   * no symbols.
   */
  def getSymbols(source: Source): Option[Vector[DocumentSymbol]] =
    None

  /**
   * The parameters are passed as an array, potentially containing gson.Json objects or primitives.
   * The first argument is required to be { uri: String } and used to obtain the source.
   */
  def executeCommand(source: Source, executeCommandParams: ExecuteCommandParams): Option[Any] = None

}

class Services[N, T <: N, C <: Config](
  server: Server[N, T, C],
  config: C
) {

  import java.util.concurrent.CompletableFuture
  import org.eclipse.lsp4j.jsonrpc.{ CancelChecker, CompletableFutures }
  import org.eclipse.lsp4j.jsonrpc.services._
  import scala.language.implicitConversions
  import com.google.gson.JsonObject

  implicit def toJavaFunction[U, V](f: Function1[U, V]): java.util.function.Function[U, V] =
    new java.util.function.Function[U, V] {
      override def apply(t: U): V = f(t)
    }

  // Life-cycle

  /**
   * Exit status to return when the server exits. `shutdown` sets this
   * to zero to reflect proper shutdown protocol.
   */
  var exitStatus = 1

  @JsonRequest("initialize")
  def initialize(params: InitializeParams): CompletableFuture[InitializeResult] =
    CompletableFuture.completedFuture {
      server.setSettings(params.getInitializationOptions)
      val serverCapabilities = new ServerCapabilities
      serverCapabilities.setCodeActionProvider(true)
      serverCapabilities.setDefinitionProvider(true)
      serverCapabilities.setDocumentFormattingProvider(true)
      serverCapabilities.setDocumentSymbolProvider(true)
      serverCapabilities.setHoverProvider(true)
      serverCapabilities.setReferencesProvider(true)
      serverCapabilities.setTextDocumentSync(TextDocumentSyncKind.Full)
      new InitializeResult(serverCapabilities)
    }

  @JsonNotification("initialized")
  def initialized(params: InitializedParams): Unit = {
    val saveOptions = new TextDocumentSaveRegistrationOptions()
    saveOptions.setIncludeText(true)
    server.registerCapability("kiama/textDocument/didSave", "textDocument/didSave", saveOptions)
  }

  @JsonNotification("exit")
  def exit(): Unit = {
    sys.exit(exitStatus)
  }

  @JsonRequest("shutdown")
  def shutdown(): CompletableFuture[Object] = {
    CompletableFuture.completedFuture {
      exitStatus = 0
      new Object
    }
  }

  // Basic text document processing

  @JsonNotification("textDocument/didOpen")
  def didOpen(params: DidOpenTextDocumentParams): Unit = {
    val document = params.getTextDocument
    process(document.getUri, document.getText)
  }

  @JsonNotification("textDocument/didChange")
  def didChange(params: DidChangeTextDocumentParams): Unit = {
    if (server.settingBool("updateOnChange")) {
      process(params.getTextDocument.getUri, params.getContentChanges.get(0).getText)
    }
  }

  @JsonNotification("textDocument/didSave")
  def didSave(params: DidSaveTextDocumentParams): Unit = {
    process(params.getTextDocument.getUri, params.getText)
  }

  @JsonNotification("textDocument/didClose")
  def didClose(params: DidCloseTextDocumentParams): Unit = {
    server.clearDiagnostics(params.getTextDocument.getUri)
  }

  def process(uri: String, text: String): Unit = {
    server.clearDiagnostics(uri)
    server.compileString(uri, text, config)
  }

  // Text document services

  def positionOfNotification(document: TextDocumentIdentifier, position: LSPPosition): Option[Position] =
    server.sources.get(document.getUri).map(source => {
      Position(position.getLine + 1, position.getCharacter + 1, source)
    })

  @JsonNotification("textDocument/codeAction")
  def codeactions(params: CodeActionParams): CompletableFuture[Array[CodeAction]] =
    CompletableFutures.computeAsync(
      (_: CancelChecker) =>
        (
          for (
            position <- positionOfNotification(params.getTextDocument, params.getRange.getStart);
            treeActions <- server.getCodeActions(position);
            codeActions = treeActions.map {
              case server.TreeAction(name, uri, oldNode, newText) =>
                val indText = server.positions.indent(newText, oldNode)
                val textEdit = new TextEdit(server.rangeOfNode(oldNode), indText)
                val changes = Map(uri -> seqToJavaList(List(textEdit)))
                val workspaceEdit = new WorkspaceEdit(mapToJavaMap(changes))
                val action = new CodeAction(name)
                action.setKind(CodeActionKind.Refactor)
                action.setEdit(workspaceEdit)
                action
            }
          ) yield codeActions.toArray
        ).getOrElse(null)
    )

  @JsonNotification("textDocument/definition")
  def definition(params: TextDocumentPositionParams): CompletableFuture[Location] =
    CompletableFutures.computeAsync(
      (_: CancelChecker) =>
        (
          for (
            position <- positionOfNotification(params.getTextDocument, params.getPosition);
            definition <- server.getDefinition(position);
            location = server.locationOfNode(definition)
          ) yield location
        ).getOrElse(null)
    )

  @JsonNotification("textDocument/documentSymbol")
  def symbols(params: DocumentSymbolParams): CompletableFuture[Array[DocumentSymbol]] =
    CompletableFutures.computeAsync(
      (_: CancelChecker) =>
        (
          for (
            source <- server.sources.get(params.getTextDocument.getUri);
            symbols <- server.getSymbols(source)
          ) yield symbols.toArray
        ).getOrElse(Array())
    )

  @JsonNotification("textDocument/formatting")
  def formatting(params: DocumentFormattingParams): CompletableFuture[Array[TextEdit]] =
    CompletableFutures.computeAsync(
      (_: CancelChecker) =>
        (
          for (
            source <- server.sources.get(params.getTextDocument.getUri);
            formatted <- server.getFormatted(source);
            start = new LSPPosition(0, 0);
            finish = new LSPPosition(source.lineCount, 0);
            edit = new TextEdit(new LSPRange(start, finish), formatted)
          ) yield Array(edit)
        ).getOrElse(null)
    )

  def hoverMarkup(markdown: String): Hover = {
    val markup = new MarkupContent()
    markup.setValue(markdown)
    markup.setKind("markdown")
    new Hover(markup)
  }

  @JsonNotification("textDocument/hover")
  def hover(params: TextDocumentPositionParams): CompletableFuture[Hover] =
    CompletableFutures.computeAsync(
      (_: CancelChecker) =>
        (
          for (
            position <- positionOfNotification(params.getTextDocument, params.getPosition);
            markdown <- server.getHover(position)
          ) yield hoverMarkup(markdown)
        ).getOrElse(null)
    )

  @JsonNotification("textDocument/references")
  def references(params: ReferenceParams): CompletableFuture[Array[Location]] =
    CompletableFutures.computeAsync(
      (_: CancelChecker) =>
        (
          for (
            position <- positionOfNotification(params.getTextDocument, params.getPosition);
            references <- server.getReferences(position, params.getContext.isIncludeDeclaration);
            locations = references.map(server.locationOfNode(_))
          ) yield locations.toArray
        ).getOrElse(null)
    )

  @JsonNotification("workspace/executeCommand")
  def commands(params: ExecuteCommandParams): CompletableFuture[Any] =
    CompletableFuture.completedFuture {
      (for {
        firstArg <- params.getArguments.toArray.headOption
        uri <- try {
          // ad-hoc parsing of json:
          // we require that the first argument is a json object with a field { uri: String }
          Some(firstArg.asInstanceOf[JsonObject].getAsJsonPrimitive("uri").getAsString)
        } catch { case e: Throwable => None }
        src <- server.sources.get(uri)
        res <- server.executeCommand(src, params)
      } yield res).getOrElse(null)
    }

  // Workspace services

  @JsonNotification("workspace/didChangeConfiguration")
  def didChangeConfiguration(params: DidChangeConfigurationParams): Unit = {
    server.setSettings(params.getSettings)
  }

  // Missing services not supported by LSP4J but sent by client side

  @JsonNotification("$/setTraceNotification")
  def setTrace(params: SetTraceNotificationParams): Unit = {
    // Do nothing
  }

}

class SetTraceNotificationParams(value: String = "off")
