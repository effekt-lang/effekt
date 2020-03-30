package effekt

import effekt.context.Context
import effekt.core.PrettyPrinter
import effekt.source.{ ModuleDecl, Tree }
import effekt.symbols.Module
import org.bitbucket.inkytonik.kiama
import kiama.util.{ Position, Source }
import org.bitbucket.inkytonik.kiama.output.PrettyPrinterTypes.Document

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
}

object Server extends LSPServer
