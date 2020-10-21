package effekt.generator

import effekt.context.Context
import effekt.core.ToplevelDecl
import effekt.symbols.moduleName
import org.bitbucket.inkytonik.kiama.output.PrettyPrinterTypes.Document

import scala.language.implicitConversions

class JavaScriptVirtual extends JavaScript {
  override val prettyPrinter: JavaScriptPrinter = new JSVirtualPrinter {}
}

trait JSVirtualPrinter extends JavaScriptPrinter {
  override def format(t: ToplevelDecl)(implicit C: Context): Document =
    pretty(virtual(t))

  def virtual(m: ToplevelDecl)(implicit C: Context): Doc = {
    val deps = m.imports
    val imports = vsep(deps.map { i =>
      "const" <+> moduleName(i) <+> "=" <+> jsCall("load", "'" + i + "'")
    }, semi)

    imports <> emptyline <> toDoc(m)
  }
}
