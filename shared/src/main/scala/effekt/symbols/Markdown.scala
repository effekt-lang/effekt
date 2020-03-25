//package effekt
//package symbols
//
//import org.bitbucket.inkytonik.kiama.output.ParenPrettyPrinter
//
//import scala.language.implicitConversions
//
//class Markdown extends ParenPrettyPrinter {
//
//  import org.bitbucket.inkytonik.kiama.output.PrettyPrinterTypes.Document
//
//  def format(t: Type): Document =
//        pretty(toDoc(t), 2)
//
//  def toDoc(t: Type): Doc = ???
//
//  def toDoc(t: ValueType): Doc = ???
//
//  def toDoc(t: BlockType): Doc = ???
//
//  def toDoc(t: Effectful): Doc = t match {
//    case Effectful(tpe, eff) if eff.isEmpty => toDoc(tpe)
//    case Effectful(tpe, eff) => toDoc(tpe) <+> "/" <+> toDoc(eff))
//  }
//
//  def toDoc(e: Effects): Doc =
//    braces(hsep(e.effs map toDoc), comma)
//
//  def toDoc(e: Effect): Doc = e.toString
//
//}