package effekt.generator

import effekt.context.Context
import effekt.core.ModuleDecl
import kiama.output.PrettyPrinterTypes.Document

import scala.language.implicitConversions

object JavaScriptVirtual extends JavaScript {
  override val prettyPrinter: JavaScriptPrinter = new JSVirtualPrinter {}
}

trait JSVirtualPrinter extends JavaScriptPrinter {
  override def format(t: ModuleDecl)(implicit C: Context): Document =
    pretty(virtual(t))

  def virtual(m: ModuleDecl)(implicit C: Context): Doc = {
    val deps = m.imports
    val imports = vsep(deps.map { i =>
      "const" <+> jsModuleName(i) <+> "=" <+> jsCall("load", "'" + moduleFile(i) + "'")
    }, semi)

    imports <> emptyline <> toDoc(m)
  }
}
