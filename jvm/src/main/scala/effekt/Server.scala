package effekt

import effekt.context.Context
import effekt.core.PrettyPrinter
import effekt.source.{ ModuleDecl, Tree }
import effekt.symbols.Module
import org.bitbucket.inkytonik.kiama
import kiama.util.{ Position, Source }
import org.bitbucket.inkytonik.kiama.output.PrettyPrinterTypes.Document
import org.eclipse.lsp4j.{ DocumentSymbol, SymbolKind }

trait LSPServer extends Driver with Intelligence {

  object prettyCore extends PrettyPrinter

  import effekt.symbols._

  import org.eclipse.lsp4j.{ Location, Range => LSPRange }

  override def getDefinition(position: Position): Option[Tree] =
    getDefinitionAt(position)(context)

  def maybeExplain(explanation: String): String =
    if (!settingBool("showExplanations")) "" else explanation.stripMargin('|')

  /**
   * Overriding backend to also publish core and target for LSP server
   */
  override def backend(mod: Module)(implicit C: Context): Option[Document] = transformer(mod) flatMap { core =>

    if (C.config.server() && settingBool("showCore")) {
      publishProduct(mod.source, "target", "effekt", prettyCore.format(core))
    }

    codegen(core) map { js =>
      if (C.config.server() && settingBool("showTarget")) {
        publishProduct(mod.source, "target", "js", js)
      }
      js
    }
  }

  override def getHover(position: Position): Option[String] = for {
    (tree, sym) <- getSymbolAt(position)(context)
    info <- getInfoOf(sym)(context)
  } yield if (settingBool("showExplanations")) info.fullDescription else info.shortDescription

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

  override def getSymbols(source: Source): Option[Vector[DocumentSymbol]] = Some(for {
    sym <- context.sources.keys
    mod = context.owner(sym)
    if mod.source == source
    id <- context.sources.get(sym)
    decl = id // TODO for now we use id as the declaration. This should be improved in SymbolsDB
    kind <- getSymbolKind(sym)
    detail <- getInfoOf(sym)(context)
  } yield new DocumentSymbol(sym.name.name, kind, rangeOfNode(decl), rangeOfNode(id), detail.header))

  def getSymbolKind(sym: Symbol): Option[SymbolKind] =
    sym match {
      case _: Module =>
        Some(SymbolKind.Class)
      case _: Fun =>
        Some(SymbolKind.Method)
      case _: Param | _: ValBinder | _: VarBinder =>
        Some(SymbolKind.Variable)
      case _ =>
        None
    }
}

object Server extends LSPServer
