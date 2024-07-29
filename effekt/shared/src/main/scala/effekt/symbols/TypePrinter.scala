package effekt
package symbols

import effekt.symbols.builtins.*
import effekt.typer.ConcreteEffects
import kiama.output.ParenPrettyPrinter

import scala.language.implicitConversions

object TypePrinter extends ParenPrettyPrinter {

  import kiama.output.PrettyPrinterTypes.Document

  val debug = false

  def show(id: source.IdDef): String = id.name
  def show(id: source.IdRef): String = (id.path :+ id.name).mkString("::")

  def show(n: Name): String = n match {
    case name: NoName.type  => name.name
    case name: LocalName    => name.name
    case name: QualifiedName => name.qualifiedName
  }
  def show(t: Type): String = pretty(toDoc(t), 80).layout
  def show(t: Capture): String = pretty(toDoc(t), 80).layout
  def show(t: Captures): String = pretty(toDoc(t), 80).layout
  def show(t: Effects): String = pretty(toDoc(t), 80).layout
  def show(t: List[ValueType | TypeVar]): String = pretty(maybeTypeParams(t), 80).layout

  val show: PartialFunction[Any, String] = {
    case id: source.IdRef   => TypePrinter.show(id)
    case id: source.IdDef   => TypePrinter.show(id)
    case n: Name            => TypePrinter.show(n)
    case t: symbols.Type    => TypePrinter.show(t)
    case t: Capture         => TypePrinter.show(t)
    case t: Captures        => TypePrinter.show(t)
    case t: Effects         => TypePrinter.show(t)
    case t: ConcreteEffects => TypePrinter.show(t.toEffects)
    case c: TypeConstructor => c.name.name
    case c: Constructor     => c.name.name
    case c: Field           => c.name.name
    case c: Operation       => c.name.name
    case p: TrackedParam    => p.name.name
    case p: ValueParam      => p.name.name
  }

  def toDoc(m: Type): Doc = m match {
    case tpe: ValueType => toDoc(tpe)
    case tpe: BlockType => toDoc(tpe)
  }

  def toDoc(tpe: ValueType): Doc = tpe match {
    case BoxedType(tpe, capture)    => toDoc(tpe) <+> "at" <+> toDoc(capture)
    case ValueTypeApp(tpe, Nil)     => tpe.name
    case ValueTypeApp(tpe, args)    => tpe.name <> brackets(hsep(args.map(toDoc), comma))
    case ValueTypeRef(x)            => toDoc(x)
  }

  def toDoc(tpe: TypeVar): Doc = tpe match {
    case typeVar: UnificationVar => typeVar.toString
    case typeVar: TypeVar => typeVar.name
  }

  def toDoc(tpe: BlockType): Doc = tpe match {
    case FunctionType(tparams, cparams, vparams, bparams, result, effects) =>
      val tps = if (tparams.isEmpty) emptyDoc else typeParams(tparams)
      val ps: Doc = (vparams, bparams) match {
        case (Nil, Nil)       => "()"
        case (List(tpe), Nil) => if (tparams.isEmpty) toDoc(tpe) else parens(toDoc(tpe))
        case (_, _) =>
          val vps = if (vparams.isEmpty) emptyDoc else parens(hsep(vparams.map(toDoc), comma))
          val bps = if (bparams.isEmpty) emptyDoc else hcat(bparams.map(toDoc).map(braces))
          vps <> bps
      }
      val ret = toDoc(result)
      val eff = if (effects.isEmpty) emptyDoc else space <> "/" <+> toDoc(effects)
      tps <> ps <+> "=>" <+> ret <> eff

    case InterfaceType(tpe, Nil)  => toDoc(tpe)
    case InterfaceType(tpe, args) => toDoc(tpe) <> typeParams(args)
  }

  def toDoc(t: BlockTypeConstructor): Doc = t match {
    case Interface(name, tparams, ops) => name
    case ExternInterface(name, tparams) => name
  }

  def toDoc(t: TypeConstructor): Doc = t match {
    case DataType(name, tparams, constructors)  => name <> typeParams(tparams)
    case Record(name, tparams, constructor) => name <> typeParams(tparams)
    case ExternType(name, tparams) => name
  }

  def toDoc(eff: Effects): Doc =
    if (eff.isEmpty) "{}" else
    braces(space <> hsep(eff.effects.map(toDoc), comma) <> space)

  def toDoc(c: Captures): Doc = c match {
    case CaptureSet(captures)  => braces { hsep(captures.toList.map(toDoc), comma) }
    case c: CaptUnificationVar => if (debug) c.name <> c.id.toString else c.name
  }

  def toDoc(c: Capture): Doc = c.name

  implicit def toDoc(name: Name): Doc = name.name

  def typeParams(tparams: List[ValueType | TypeVar]): Doc = brackets(hsep(tparams.map {
    case tpe: ValueType => toDoc(tpe)
    case tpe: TypeVar => toDoc(tpe)
  }, comma))

  def maybeTypeParams(tparams: List[ValueType | TypeVar]): Doc =
    if (tparams.isEmpty) "" else typeParams(tparams)

}

implicit class ErrorMessageInterpolator(private val sc: StringContext) extends AnyVal {
  def pp(args: Any*): String = sc.s(args.map(effekt.util.show): _*)
}

