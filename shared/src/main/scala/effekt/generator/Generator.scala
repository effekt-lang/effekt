package effekt
package generator

import effekt.context.Context
import effekt.symbols.LegacyModule
import org.bitbucket.inkytonik.kiama
import kiama.output.PrettyPrinterTypes.Document
import kiama.util.Source

trait Generator extends Phase[Source, Document] {

  /**
   * A Unix path that is *not* platform dependent.
   */
  def path(m: LegacyModule)(implicit C: Context): String

}
