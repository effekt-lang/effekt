package effekt
package symbols

import effekt.symbols.builtins.*
import kiama.output.ParenPrettyPrinter

import scala.language.implicitConversions

object TypePrinter extends ParenPrettyPrinter {

  import kiama.output.PrettyPrinterTypes.Document

  val debug = false

  def show(id: source.Id): String = id.name
  def show(n: Name): String = n.name
  def show(t: Type): String = pretty(toDoc(t), 80).layout
  def show(t: Capture): String = pretty(toDoc(t), 80).layout
  def show(t: Captures): String = pretty(toDoc(t), 80).layout
  def show(t: Effects): String = pretty(toDoc(t), 80).layout

  def toDoc(m: Type): Doc = m match {
    case tpe: ValueType => toDoc(tpe)
    case tpe: BlockType => toDoc(tpe)
  }

  def toDoc(tpe: ValueType): Doc = tpe match {
    case BoxedType(tpe, capture)    => toDoc(tpe) <+> "at" <+> toDoc(capture)
    case typeVar: UnificationVar    => typeVar.toString
    case typeVar: TypeVar           => typeVar.name
    case ValueTypeApp(tpe, args)    => toDoc(tpe) <> brackets(hsep(args.map(toDoc), comma))
    case c: TypeConstructor         => c.name
    case BuiltinType(name, tparams) => name
  }

  def toDoc(tpe: BlockType): Doc = tpe match {
    case FunctionType(tparams, cparams, vparams, bparams, result, effects) =>
      val tps = if (tparams.isEmpty) emptyDoc else typeParams(tparams)
      val ps: Doc = (vparams, bparams) match {
        case (Nil, Nil)       => "()"
        case (List(tpe), Nil) => toDoc(tpe)
        case (_, _) =>
          val vps = if (vparams.isEmpty) emptyDoc else parens(hsep(vparams.map(toDoc), comma))
          val bps = if (bparams.isEmpty) emptyDoc else hcat(bparams.map(toDoc).map(braces))
          vps <> bps
      }
      val ret = toDoc(result)
      val eff = if (effects.isEmpty) emptyDoc else space <> "/" <+> toDoc(effects)
      tps <> ps <+> "=>" <+> ret <> eff

    case BlockTypeApp(tpe, args)       => toDoc(tpe) <> typeParams(args)
    case Interface(name, tparams, ops) => name
    case BuiltinEffect(name, tparams)  => name
  }

  def toDoc(t: TypeConstructor): Doc = t match {
    case DataType(name, tparams, variants)  => name <> typeParams(tparams)
    case Record(name, tparams, tpe, fields) => name <> typeParams(tparams)
  }

  def toDoc(eff: Effects): Doc = if (eff.isEmpty) "{}" else braces(space <> hsep(eff.effects.map(toDoc), comma) <> space)

  def toDoc(c: Captures): Doc = c match {
    case CaptureSet(captures)  => braces { hsep(captures.toList.map(toDoc), comma) }
    case c: CaptUnificationVar => if (debug) c.name <> c.id.toString else c.name
  }

  def toDoc(c: Capture): Doc = c.name

  implicit def toDoc(name: Name): Doc = name.name

  def typeParams(tparams: List[ValueType]): Doc = brackets(hsep(tparams.map(toDoc), comma))

}

implicit class ErrorMessageInterpolator(private val sc: StringContext) extends AnyVal {
  def pp(args: Any*): String = sc.s(args.map {
    case id: source.Id   => TypePrinter.show(id)
    case name: Name      => name.name
    case t: symbols.Type => TypePrinter.show(t)
    case t: Capture      => TypePrinter.show(t)
    case t: Captures     => TypePrinter.show(t)
    case t: Effects      => TypePrinter.show(t)
    case other           => other.toString
  }: _*)
}
