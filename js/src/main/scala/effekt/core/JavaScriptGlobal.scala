package effekt
package core

import org.bitbucket.inkytonik.kiama.output.ParenPrettyPrinter
import effekt.context.Context

import scala.language.implicitConversions
import effekt.symbols.{ Name, builtins, moduleFile, moduleName }
import effekt.target.JavaScript
import org.bitbucket.inkytonik.kiama.output.PrettyPrinterTypes.Document

class JavaScriptGlobal extends JavaScript {

  override def format(t: ModuleDecl)(implicit C: Context): Document =
    pretty(global(t))

  /**
   * No JS module system.
   *
   * Defines the given module as top level global variable.
   */
  def global(m: ModuleDecl)(implicit C: Context): Doc = {
    "var" <+> moduleName(m.path) <+> "=" <+> parens("function()" <+> braces(
      nest(line <> toDoc(m)) <> line
    )) <> ".apply(this)"
  }
}
