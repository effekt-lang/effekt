package effekt

import com.google.gson.JsonElement
import kiama.util.Convert.*
import effekt.context.Context
import effekt.source.Def.FunDef
import effekt.source.Term.Hole
import effekt.source.Tree
import effekt.symbols.Binder.{ValBinder, VarBinder}
import effekt.symbols.BlockTypeConstructor.{ExternInterface, Interface}
import effekt.symbols.TypeConstructor.{DataType, ExternType}
import effekt.symbols.{Anon, Binder, Callable, Effects, Module, Param, Symbol, TypeAlias, TypePrinter, UserFunction, ValueType, isSynthetic}
import effekt.util.{PlainMessaging, PrettyPrinter}
import effekt.util.messages.EffektError
import kiama.util.Collections.{mapToJavaMap, seqToJavaList}
import kiama.util.{Collections, Convert, Position, Source}
import org.eclipse.lsp4j.jsonrpc.services.JsonNotification
import org.eclipse.lsp4j.jsonrpc.{Launcher, messages}
import org.eclipse.lsp4j.launch.LSPLauncher
import org.eclipse.lsp4j.services.*
import org.eclipse.lsp4j.{CodeAction, CodeActionKind, CodeActionParams, Command, DefinitionParams, Diagnostic, DidChangeConfigurationParams, DidChangeTextDocumentParams, DidChangeWatchedFilesParams, DidCloseTextDocumentParams, DidOpenTextDocumentParams, DidSaveTextDocumentParams, DocumentSymbol, DocumentSymbolParams, Hover, HoverParams, InitializeParams, InitializeResult, InlayHint, InlayHintKind, InlayHintParams, Location, LocationLink, MarkupContent, MessageParams, MessageType, PublishDiagnosticsParams, ReferenceParams, SaveOptions, ServerCapabilities, SetTraceParams, SymbolInformation, SymbolKind, TextDocumentSaveRegistrationOptions, TextDocumentSyncKind, TextDocumentSyncOptions, TextEdit, WorkspaceEdit, Range as LSPRange}

import java.io.{InputStream, OutputStream, PrintWriter}
import java.net.ServerSocket
import java.nio.file.Paths
import java.util
import java.util.concurrent.{CompletableFuture, ExecutorService, Executors}

/**
 * Effekt Language Server
 *
 * @param compileOnChange Whether to compile on `didChange` events
 *                        Currently disabled because references are erased when there are any compiler errors.
 *                        Therefore, we currently only update on `didSave` until we have working caching for references.
 */
class Server(config: EffektConfig, compileOnChange: Boolean=false) extends LanguageServer with Driver with Intelligence with TextDocumentService with WorkspaceService {
  private var client: EffektLanguageClient = _
  private val textDocumentService = this
  private val workspaceService = this

  // Track whether shutdown has been requested
  private var shutdownRequested: Boolean = false
  // Configuration sent by the language client
  var settings: JsonElement = null

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
    capabilities.setInlayHintProvider(true)

    // We need to explicitly ask the client to include the text on save events.
    // Otherwise, when not listening to `didChange`, we have no way to get the text of the file,
    // when the client decides not to include the text in the `didSave` event.
    val saveOptions = new SaveOptions()
    saveOptions.setIncludeText(true)

    val syncOptions = new TextDocumentSyncOptions();
    syncOptions.setOpenClose(true);
    syncOptions.setChange(TextDocumentSyncKind.Full);
    syncOptions.setSave(saveOptions);
    capabilities.setTextDocumentSync(syncOptions);

    // Load the initial settings from client-sent `initializationOptions` (if any)
    // This is not part of the LSP standard, but seems to be the most reliable way to have the correct initial settings
    // on first compile.
    // There is a `workspace/didChangeConfiguration` notification and a `workspace/configuration` request, but we cannot
    // guarantee that the client will send these before the file is first compiled, leading to either duplicate work
    // or a bad user experience.
    if (params.getInitializationOptions != null)
      workspaceService.didChangeConfiguration(new DidChangeConfigurationParams(params.getInitializationOptions))

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

  override def setTrace(params: SetTraceParams): Unit = {
    // Do nothing
  }

  // The LSP services are also implemented by the Server class as they are strongly coupled anyway.
  override def getTextDocumentService(): TextDocumentService = this
  override def getWorkspaceService(): WorkspaceService = this

  // LSP Diagnostics
  //
  //

  def clearDiagnostics(name: String): Unit = {
    publishDiagnostics(name, Vector())
  }

  def publishDiagnostics(name: String, diagnostics: Vector[Diagnostic]): Unit = {
    val params = new PublishDiagnosticsParams(Convert.toURI(name), Collections.seqToJavaList(diagnostics))
    client.publishDiagnostics(params)
  }

  // Custom Effekt extensions
  //
  //

  /**
   * Publish Effekt IR for a given source file
   *
   * @param source The Kiama source file
   * @param config The Effekt configuration
   * @param C The compiler context
   */
  def publishIR(source: Source, config: EffektConfig)(implicit C: Context): Unit = {
    // Publish Effekt IR
    val showIR = workspaceService.settingString("showIR").getOrElse("none")
    val showTree = workspaceService.settingBool("showTree")

    if (showIR == "none") {
      return;
    }

    if (showIR == "source") {
      val tree = C.compiler.getAST(source)
      if (tree.isEmpty) return
      client.publishIR(EffektPublishIRParams(
        basename(source.name) + ".scala",
        PrettyPrinter.format(tree.get).layout
      ))
      return;
    }

    def unsupported =
      throw new RuntimeException(s"Combination of '${showIR}' and showTree=${showTree} not supported by backend ${C.config.backend().name}")

    val stage = showIR match {
      case "core" => Stage.Core
      case "machine" => Stage.Machine
      case "target" => Stage.Target
      case _ => unsupported
    }

    if (showTree) {
      client.publishIR(EffektPublishIRParams(
        basename(source.name) + ".scala",
        PrettyPrinter.format(C.compiler.treeIR(source, stage).getOrElse(unsupported)).layout
      ))
    } else if (showIR == "target") {
      client.publishIR(EffektPublishIRParams(
        basename(source.name) + "." + C.runner.extension,
        C.compiler.prettyIR(source, Stage.Target).getOrElse(unsupported).layout
      ))
    } else {
      client.publishIR(EffektPublishIRParams(
        basename(source.name) + ".ir",
        C.compiler.prettyIR(source, stage).getOrElse(unsupported).layout
      ))
    }
  }

  // Driver methods
  //
  //

  override def afterCompilation(source: Source, config: EffektConfig)(implicit C: Context): Unit = {
    // Publish LSP diagnostics
    val messages = C.messaging.buffer
    val groups = messages.groupBy(msg => msg.sourceName.getOrElse(""))
    for ((name, msgs) <- groups) {
      publishDiagnostics(name, msgs.distinct.map(Convert.messageToDiagnostic(lspMessaging)))
    }
    try {
      publishIR(source, config)
    } catch {
      case e => client.logMessage(new MessageParams(MessageType.Error, e.toString + ":" + e.getMessage))
    }
  }

  // Other methods
  //
  //

  def basename(filename: String): String = {
    val name = Paths.get(filename).getFileName.toString
    val dotIndex = name.lastIndexOf('.')
    if (dotIndex > 0) name.substring(0, dotIndex) else name
  }

  def connect(client: EffektLanguageClient): Unit = {
    this.client = client
  }

  /**
   * Launch a language server using provided input/output streams.
   * This allows tests to connect via in-memory pipes.
   */
  def launch(client: EffektLanguageClient, in: InputStream, out: OutputStream): Launcher[EffektLanguageClient] = {
    val executor = Executors.newSingleThreadExecutor()
    val launcher =
      new LSPLauncher.Builder()
        .setLocalService(this)
        .setRemoteInterface(classOf[EffektLanguageClient])
        .setInput(in)
        .setOutput(out)
        .setExecutorService(executor)
        .create()
    this.connect(client)
    launcher.startListening()
    launcher
  }

  // LSP Document Lifecycle
  //
  //

  def didChange(params: DidChangeTextDocumentParams): Unit = {
    if (!compileOnChange) return
    val document = params.getTextDocument
    clearDiagnostics(document.getUri)
    getDriver.compileString(document.getUri, params.getContentChanges.get(0).getText, getConfig)
  }

  def didClose(params: DidCloseTextDocumentParams): Unit = {
    clearDiagnostics(params.getTextDocument.getUri)
  }

  def didOpen(params: DidOpenTextDocumentParams): Unit = {
    val document = params.getTextDocument
    clearDiagnostics(document.getUri)
    getDriver.compileString(document.getUri, document.getText, getConfig)
  }

  def didSave(params: DidSaveTextDocumentParams): Unit = {
    val document = params.getTextDocument
    val text = Option(params.getText) match {
      case Some(t) => t
      case None =>
        return
    }
    clearDiagnostics(document.getUri)
    getDriver.compileString(document.getUri, text, getConfig)
  }

  // LSP Hover
  //
  //

  override def hover(params: HoverParams): CompletableFuture[Hover] = {
    val position = sources.get(params.getTextDocument.getUri).map { source =>
      Convert.fromLSPPosition(params.getPosition, source)
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
    (tree, sym) <- getSymbolAt(position)(using context)
    info <- getInfoOf(sym)(using context)
  } yield if (settingBool("showExplanations")) info.fullDescription else info.shortDescription

  def getHoleHover(position: Position): Option[String] = for {
    trees <- getTreesAt(position)(using context)
    tree <- trees.collectFirst { case h: source.Hole => h }
    info <- getHoleInfo(tree)(using context)
  } yield info

  // LSP Document Symbols
  //
  //

  override def documentSymbol(params: DocumentSymbolParams): CompletableFuture[util.List[messages.Either[SymbolInformation, DocumentSymbol]]] = {
    val source = sources.get(params.getTextDocument.getUri)
    if (source.isEmpty) return CompletableFuture.completedFuture(Collections.seqToJavaList(Vector()))

    context.compiler.runFrontend(source.get)(using context)

    val documentSymbols = for {
      sym <- context.sourceSymbolsFor(source.get).toVector
      if !sym.isSynthetic
      id <- context.definitionTreeOption(sym)
      decl <- getSourceTreeFor(sym)
      kind <- getSymbolKind(sym)
      detail <- getInfoOf(sym)(using context)
      declRange = convertRange(positions.getStart(decl), positions.getFinish(decl))
      idRange = convertRange(positions.getStart(id), positions.getFinish(id))
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
    case _ => context.definitionTreeOption(sym)
  }

  def getSymbolKind(sym: Symbol): Option[SymbolKind] =
    sym match {
      case _: Module =>
        Some(SymbolKind.Module)
      case _: Interface | _: ExternInterface =>
        Some(SymbolKind.Interface)
      case _: DataType | _: ExternType | _: TypeAlias =>
        Some(SymbolKind.Enum)
      case _: Callable =>
        Some(SymbolKind.Method)
      case _: Param | _: ValBinder | _: VarBinder =>
        Some(SymbolKind.Variable)
      case _ =>
        None
    }

  // LSP Go To Definition
  //
  //

  override def definition(params: DefinitionParams): CompletableFuture[messages.Either[util.List[_ <: Location], util.List[_ <: LocationLink]]] = {
    val location = for {
      position <- sources.get(params.getTextDocument.getUri).map { source =>
        fromLSPPosition(params.getPosition, source)
      };
      definition <- getDefinitionAt(position)(using context);
      location = locationOfNode(positions, definition)
    } yield location

    val result = location.map(l => messages.Either.forLeft[util.List[_ <: Location], util.List[_ <: LocationLink]](Collections.seqToJavaList(List(l))))
      .getOrElse(messages.Either.forLeft(Collections.seqToJavaList(List())))

    CompletableFuture.completedFuture(result)
  }

  // LSP References
  //
  //

  override def references(params: ReferenceParams): CompletableFuture[util.List[_ <: Location]] = {
    val position = sources.get(params.getTextDocument.getUri).map { source =>
      fromLSPPosition(params.getPosition, source)
    }
    if (position.isEmpty)
      return CompletableFuture.completedFuture(Collections.seqToJavaList(Vector()))

    val locations = for {
      (tree, sym) <- getSymbolAt(position.get)(using context)
      refs = context.distinctReferencesTo(sym)
      // getContext may be null!
      includeDeclaration = Option(params.getContext).exists(_.isIncludeDeclaration)
      allRefs = if (includeDeclaration) tree :: refs else refs
      locations = allRefs.map(ref => locationOfNode(positions, ref))
    } yield locations

    CompletableFuture.completedFuture(Collections.seqToJavaList(locations.getOrElse(Seq[Location]())))
  }

  // LSP Inlay Hints
  //
  //

  override def inlayHint(params: InlayHintParams): CompletableFuture[util.List[InlayHint]] = {
    val hints = for {
      source <- sources.get(params.getTextDocument.getUri)
      hints = {
        val range = fromLSPRange(params.getRange, source)
        getInferredCaptures(range)(using context).map {
          case (p, c) =>
            val prettyCaptures = TypePrinter.show(c)
            val inlayHint = new InlayHint(convertPosition(p), messages.Either.forLeft(prettyCaptures))
            inlayHint.setKind(InlayHintKind.Type)
            val markup = new MarkupContent()
            markup.setValue(s"captures: `${prettyCaptures}`")
            markup.setKind("markdown")
            inlayHint.setTooltip(markup)
            inlayHint.setPaddingRight(true)
            inlayHint.setData("capture")
            inlayHint
        }.toVector
      }
    } yield hints

    CompletableFuture.completedFuture(Collections.seqToJavaList(hints.getOrElse(Vector())))
  }

  // LSP Code Actions
  //
  //

  // FIXME: This is the code actions code from the previous language server implementation.
  // It doesn't even work in the previous implementation.
  override def codeAction(params: CodeActionParams): CompletableFuture[util.List[messages.Either[Command, CodeAction]]] = {
    val codeActions = for {
      position <- sources.get(params.getTextDocument.getUri).map { source =>
        fromLSPPosition(params.getRange.getStart, source)
      };
      codeActions = for {
        trees <- getTreesAt(position)(using context).toVector
        actions <- trees.flatMap { t => action(t)(using context) }
      } yield actions
    } yield codeActions.toList

    val result = codeActions.getOrElse(List[CodeAction]()).map(messages.Either.forRight[Command, CodeAction])
    CompletableFuture.completedFuture(Collections.seqToJavaList(result))
  }

  def action(tree: Tree)(using C: Context): Option[CodeAction] = tree match {
    case f: FunDef => inferEffectsAction(f)
    case h: Hole => closeHoleAction(h)
    case _ => None
  }

  def EffektCodeAction(description: String, oldNode: Any, newText: String): Option[CodeAction] = {
    for {
      posFrom <- positions.getStart(oldNode)
      posTo <- positions.getFinish(oldNode)
    } yield {
      val textEdit = new TextEdit(convertRange(Some(posFrom), Some(posTo)), newText)
      val changes = Map(posFrom.source.name -> seqToJavaList(List(textEdit)))
      val workspaceEdit = new WorkspaceEdit(mapToJavaMap(changes))
      val action = new CodeAction(description)
      action.setKind(CodeActionKind.Refactor)
      action.setEdit(workspaceEdit)
      action
    }
  }

  /**
   * FIXME: The following comment was left on the previous Kiama-based implementation and can now be addressed:
   *
   * TODO it would be great, if Kiama would allow setting the position of the code action separately
   * from the node to replace. Here, we replace the annotated return type, but would need the
   * action on the function (since the return type might not exist in the original program).
   *
   * Also, it is necessary to be able to manually set the code action kind (and register them on startup).
   * This way, we can use custom kinds like `refactor.closehole` that can be mapped to keys.
   */
  def inferEffectsAction(fun: FunDef)(using C: Context): Option[CodeAction] = for {
    // the inferred type
    (tpe, eff) <- C.inferredTypeAndEffectOption(fun)
    // the annotated type
    ann = for {
      result <- fun.symbol.annotatedResult
      effects <- fun.symbol.annotatedEffects
    } yield (result, effects)
    if ann.map {
      needsUpdate(_, (tpe, eff))
    }.getOrElse(true)
    res <- EffektCodeAction("Update return type with inferred effects", fun.ret, s": $tpe / $eff")
  } yield res

  def closeHoleAction(hole: Hole)(using C: Context): Option[CodeAction] = for {
    holeTpe <- C.inferredTypeOption(hole)
    contentTpe <- C.inferredTypeOption(hole.stmts)
    if holeTpe == contentTpe
    res <- hole match {
      case Hole(source.Return(exp)) => for {
        text <- positions.textOf(exp)
        res <- EffektCodeAction("Close hole", hole, text)
      } yield res

      // <{ s1 ; s2; ... }>
      case Hole(stmts) => for {
        text <- positions.textOf(stmts)
        res <- EffektCodeAction("Close hole", hole, s"locally { ${text} }")
      } yield res
    }
  } yield res

  def needsUpdate(annotated: (ValueType, Effects), inferred: (ValueType, Effects))(using Context): Boolean = {
    val (tpe1, effs1) = annotated
    val (tpe2, effs2) = inferred
    tpe1 != tpe2 || effs1 != effs2
  }

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

  def settingString(name: String): Option[String] = {
    if (settings == null) return None
    val obj = settings.getAsJsonObject
    if (obj == null) return None
    val value = obj.get(name)
    if (value == null) return None
    Some(value.getAsString)
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
          .setRemoteInterface(classOf[EffektLanguageClient])
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
          .setRemoteInterface(classOf[EffektLanguageClient])
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

case class ServerConfig(debug: Boolean = false, debugPort: Int = 5000)

trait EffektLanguageClient extends LanguageClient {
  /**
   * Custom LSP notification to publish Effekt IR
   *
   * @param params The parameters for the notification
   */
  @JsonNotification("$/effekt/publishIR")
  def publishIR(params: EffektPublishIRParams): Unit
}

/**
 * Custom LSP notification to publish Effekt IR
 *
 * @param filename The filename of the resulting IR file
 * @param content The IR content
 */
case class EffektPublishIRParams(filename: String,
                                 content: String
)
