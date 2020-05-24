package effekt
package core

import org.bitbucket.inkytonik.kiama.output.ParenPrettyPrinter
import effekt.context.Context

import scala.language.implicitConversions
import effekt.symbols.{ Name, builtins, moduleFile, moduleName }
import effekt.target.JavaScript
import org.bitbucket.inkytonik.kiama.output.PrettyPrinterTypes.Document

class JavaScriptVirtual extends JavaScript {

  override def format(t: ModuleDecl)(implicit C: Context): Document =
    pretty(virtual(t))

  def virtual(m: ModuleDecl)(implicit C: Context): Doc = {
    val deps = m.imports
    val imports = vsep(deps.map { i =>
      "const" <+> moduleName(i) <+> "=" <+> jsCall("load", "'" + i + "'")
    }, semi)

    imports <> emptyline <> toDoc(m)
  }
}
