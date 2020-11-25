package effekt.generator

import effekt.context.Context
import effekt.core.SourceScope
import effekt.symbols.Name
import org.bitbucket.inkytonik.kiama.output.PrettyPrinterTypes.Document

import scala.language.implicitConversions

class JavaScriptVirtual extends JavaScript {
  override val prettyPrinter: JavaScriptPrinter = new JSVirtualPrinter {}
}

trait JSVirtualPrinter extends JavaScriptPrinter {
  override def format(t: SourceScope)(implicit C: Context): Document =
    pretty(virtual(t))

  def virtual(m: SourceScope)(implicit C: Context): Doc = {
    val deps = m.imports
    val imports = vsep(deps.map { i =>
      "const" <+> jsModuleName(i) <+> "=" <+> jsCall("load", "'" + i + "'")
    }, semi)

    imports <> emptyline <> toDoc(m)
  }
}
