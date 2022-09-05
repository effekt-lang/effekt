package effekt
package util

import kiama.output.ParenPrettyPrinter
import kiama.output.PrettyPrinterTypes.Document

import scala.language.implicitConversions

/**
 * A generic pretty printer for Scala case classes
 */
object PrettyPrinter extends ParenPrettyPrinter {

  override val defaultIndent = 2

  def format(value: Any): Document = pretty(toDoc(value), 80)

  def toDoc(value: Any): Doc = value match {
    case sym: effekt.symbols.Symbol => string(sym.name.name + "_" + sym.id.toString)
    case Nil => "Nil"
    case l: List[a] => "List" <> parens(l.map(toDoc))
    case l: Map[a, b] => "Map" <> parens(l.map {
      case (k, v) => toDoc(k) <+> "->" <+> toDoc(v)
    }.toList)
    case p: Product => p.productPrefix <> parens(p.productIterator.map(toDoc).toList)
    case other => other.toString
  }

  def parens(docs: List[Doc]): Doc = parens(group(nest(linebreak <> ssep(docs, comma <> line)) <> linebreak))
}
