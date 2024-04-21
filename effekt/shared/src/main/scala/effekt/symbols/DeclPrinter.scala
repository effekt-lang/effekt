package effekt
package symbols

import effekt.context.Context
import kiama.output.ParenPrettyPrinter

import scala.language.implicitConversions
import TypePrinter.show

object DeclPrinter extends ParenPrettyPrinter {

  override val defaultIndent = 2

  def apply(t: Symbol)(implicit context: Context): String = ""

  def toDoc(t: Symbol, context: Context): Doc = ???

  def format(kw: String, f: Callable, result: Option[ValueType], effects: Option[Effects]): Doc =  ???
}
