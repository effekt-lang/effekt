package effekt.util

import kiama.output.ParenPrettyPrinter

object GenericPrinter extends ParenPrettyPrinter {

  def apply(a: Any): String = pretty(toDoc(a)).layout

  def toDoc(a: Any): Doc = a match {
    case x if x == null => "null"
    case p: Product     => p.productPrefix <> parens(group(nest(linebreak <> hsep(p.productIterator.map(toDoc).toSeq, comma))))
    case other          => other.toString
  }
}
