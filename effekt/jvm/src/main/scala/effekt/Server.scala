package effekt

import effekt.context.Context
import effekt.core.PrettyPrinter
import effekt.lifted.LiftInference
import effekt.source.{ FunDef, Hole, ModuleDecl, Tree }
import effekt.util.{ PlainMessaging, getOrElseAborting }
import effekt.util.messages.EffektError

import kiama.util.{ Position, Services, Source }
import kiama.output.PrettyPrinterTypes.Document

import org.eclipse.lsp4j.{ Diagnostic, DocumentSymbol, SymbolKind, ExecuteCommandParams }

/**
 * effekt.Intelligence <--- gathers information -- LSPServer --- provides LSP interface ---> kiama.Server
 *     |
 *     v
 * effekt.Compiler
 */
trait LSPServer extends kiama.util.Server[Tree, ModuleDecl, EffektConfig, EffektError] with Driver with Intelligence {

  import effekt.symbols._

  import org.eclipse.lsp4j.{ Location, Range => LSPRange }

  // Diagnostics
  object lspMessaging extends PlainMessaging

  override def messageToDiagnostic(message: EffektError): Diagnostic =
    diagnostic(message.range, lspMessaging.formatContent(message), message.severity)

  override def getDefinition(position: Position): Option[Tree] =
    getDefinitionAt(position)(context)

  /**
   * Overriding hook to also publish core and target for LSP server
   */
  override def afterCompilation(source: Source, config: EffektConfig)(using C: Context): Unit = {
    super.afterCompilation(source, config)

    // don't do anything, if we aren't running as a language server
    if (!C.config.server()) return ;

    val showIR = settingStr("showIR")
    val showTree = settingBool("showTree")

    def publishTree(name: String, tree: Any): Unit =
      publishProduct(source, name, "scala", util.PrettyPrinter.format(tree))

    def publishIR(name: String, doc: Document): Unit =
      publishProduct(source, name, "ir", doc)

    if (showIR == "none") { return; }

    if (showIR == "source") {
      val tree = C.getAST(source).getOrElseAborting { return; }
      publishTree("source", tree)
    }

    if (List("target", "core", "lifted-core") contains showIR) {

      val (transformed, out) = C.compileSeparate(source).getOrElseAborting { return; }

      if (showIR == "target") {
        val extension = C.config.backend() match {
          case "js" => "js"
          case "chez-monadic" | "chez-callcc" | "chez-lift" => "ss"
          case "llvm" => "ll"
        }
        publishProduct(source, "target", extension, out)
      }

      if (showIR == "core") {
        if (showTree) publishTree("core", transformed.core)
        else publishIR("core", core.PrettyPrinter.format(transformed.core))
      }

      if (showIR == "lifted-core") {
        val liftedCore = LiftInference.run(transformed).getOrElseAborting { return; }
        if (showTree) publishTree("lifted", liftedCore.core)
        else publishIR("lifted", lifted.PrettyPrinter.format(liftedCore.core))
      }
    }

    if (showIR == "machine") {
      val CompilationUnit(main, deps) = C.compileAll(source).getOrElseAborting { return; }
      val machineProg = machine.Transformer.transform(main)

      if (showTree) publishTree("machine", machineProg.program)
      else publishIR("machine", machine.PrettyPrinter.format(machineProg.program))
    }
  }

  override def getHover(position: Position): Option[String] =
    getSymbolHover(position) orElse getHoleHover(position)

  def getSymbolHover(position: Position): Option[String] = for {
    (tree, sym) <- getSymbolAt(position)(context)
    info <- getInfoOf(sym)(context)
  } yield if (settingBool("showExplanations")) info.fullDescription else info.shortDescription

  def getHoleHover(position: Position): Option[String] = for {
    trees <- getTreesAt(position)(context)
    tree <- trees.collectFirst { case h: source.Hole => h }
    info <- getHoleInfo(tree)(context)
  } yield info

  // The implementation in kiama.Server does not support file sources
  override def locationOfNode(node: Tree): Location = {
    (positions.getStart(node), positions.getFinish(node)) match {
      case (start @ Some(st), finish @ Some(_)) =>
        val s = convertPosition(start)
        val f = convertPosition(finish)
        new Location(st.source.name, new LSPRange(s, f))
      case _ =>
        null
    }
  }

  def positionToLocation(p: Position): Location = {
    val s = convertPosition(Some(p))
    new Location(p.source.name, new LSPRange(s, s))
  }

  def getSourceTreeFor(sym: Symbol): Option[Tree] = sym match {
    case a: Anon => Some(a.decl)
    case f: UserFunction => Some(f.decl)
    case b: Binder => Some(b.decl)
    case _ => context.definitionTreeOption(sym)
  }

  override def getSymbols(source: Source): Option[Vector[DocumentSymbol]] =

    context.runFrontend(source)(using context)

    val documentSymbols = for {
      sym <- context.sourceSymbolsFor(source).toVector
      if !sym.isSynthetic
      id <- context.definitionTreeOption(sym)
      decl <- getSourceTreeFor(sym)
      kind <- getSymbolKind(sym)
      detail <- getInfoOf(sym)(context)
    } yield new DocumentSymbol(sym.name.name, kind, rangeOfNode(decl), rangeOfNode(id), detail.header)
    Some(documentSymbols)

  override def getReferences(position: Position, includeDecl: Boolean): Option[Vector[Tree]] =
    for {
      (tree, sym) <- getSymbolAt(position)(context)
      refs = context.distinctReferencesTo(sym)
      allRefs = if (includeDecl) tree :: refs else refs
    } yield allRefs.toVector

  // settings might be null
  override def setSettings(settings: Object): Unit = {
    import com.google.gson.JsonObject
    if (settings == null) super.setSettings(new JsonObject())
    else super.setSettings(settings)
  }

  //references

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

  override def getCodeActions(position: Position): Option[Vector[TreeAction]] =
    Some(for {
      trees <- getTreesAt(position)(context).toVector
      actions <- trees.flatMap { t => action(t)(context) }
    } yield actions)

  def action(tree: Tree)(implicit C: Context): Option[TreeAction] = tree match {
    case f: FunDef => inferEffectsAction(f)
    case h: Hole   => closeHoleAction(h)
    case _         => None
  }

  def CodeAction(description: String, oldNode: Any, newText: String): Option[TreeAction] =
    for {
      posFrom <- positions.getStart(oldNode)
      posTo <- positions.getFinish(oldNode)
    } yield TreeAction(description, posFrom.source.name, posFrom, posTo, newText)

  /**
   * TODO it would be great, if Kiama would allow setting the position of the code action separately
   * from the node to replace. Here, we replace the annotated return type, but would need the
   * action on the function (since the return type might not exist in the original program).
   *
   * Also, it is necessary to be able to manually set the code action kind (and register them on startup).
   * This way, we can use custom kinds like `refactor.closehole` that can be mapped to keys.
   */
  def inferEffectsAction(fun: FunDef)(using C: Context): Option[TreeAction] = for {
    // the inferred type
    (tpe, eff) <- C.inferredTypeAndEffectOption(fun)
    // the annotated type
    ann = for {
      result <- fun.symbol.annotatedResult
      effects <- fun.symbol.annotatedEffects
    } yield (result, effects)
    if ann.map { needsUpdate(_, (tpe, eff)) }.getOrElse(true)
    res <- CodeAction("Update return type with inferred effects", fun.ret, s": $tpe / $eff")
  } yield res

  def closeHoleAction(hole: Hole)(using C: Context): Option[TreeAction] = for {
    holeTpe <- C.inferredTypeOption(hole)
    contentTpe <- C.inferredTypeOption(hole.stmts)
    if holeTpe == contentTpe
    res <- hole match {
      case Hole(source.Return(exp)) => for {
        text <- positions.textOf(exp)
        res <- CodeAction("Close hole", hole, text)
      } yield res

      // <{ s1 ; s2; ... }>
      case Hole(stmts) => for {
        text <- positions.textOf(stmts)
        res <- CodeAction("Close hole", hole, s"locally { ${text} }")
      } yield res
    }
  } yield res

  def needsUpdate(annotated: (ValueType, Effects), inferred: (ValueType, Effects))(using Context): Boolean = {
    val (tpe1, effs1) = annotated
    val (tpe2, effs2) = inferred
    tpe1 != tpe2 || effs1 != effs2
  }

  case class CaptureInfo(location: Location, captureText: String)

  override def executeCommand(src: Source, params: ExecuteCommandParams): Option[Any] =
    if (params.getCommand == "inferredCaptures") {
      val captures = getInferredCaptures(src)(using context).map {
        case (p, c) => CaptureInfo(positionToLocation(p), TypePrinter.show(c))
      }
      if (captures.isEmpty) None else Some(captures.toArray)
    } else {
      None
    }

  override def createServices(config: EffektConfig) = new LSPServices(this, config)

  // Class to easily test custom LSP services not (yet) meant to go into kiama.Services
  class LSPServices(server: LSPServer, config: EffektConfig) extends Services[Tree, ModuleDecl, EffektConfig, EffektError](server, config) {}
}

/**
 * Main entry point for Effekt
 */
object Server extends LSPServer
