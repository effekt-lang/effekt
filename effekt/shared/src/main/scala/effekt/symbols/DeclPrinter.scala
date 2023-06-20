package effekt
package symbols

import effekt.context.Context
import kiama.output.ParenPrettyPrinter

import scala.language.implicitConversions
import TypePrinter.show

object DeclPrinter extends ParenPrettyPrinter {

  override val defaultIndent = 2

  def apply(t: Symbol)(implicit context: Context): String =
    pretty(toDoc(t, context)).layout

  def toDoc(t: Symbol, context: Context): Doc = t match {

    case e @ Interface(name, tparams, List(op)) =>
      format("effect", op, op.annotatedResult, op.annotatedEffects)

    case e @ Interface(name, tparams, ops) =>
      val tps = show(tparams)
      val effs = ops.map { op => format("def", op, op.annotatedResult, op.annotatedEffects) }
      "effect" <+> name.toString <> tps <+> braces(nest(line <> vsep(effs)) <> line)

    case b @ ValBinder(name, tps, decl) =>
      val tpe = context.valueTypeOption(b).getOrElse { b.tpe.get }
      pp"val ${name}: ${tpe}"

    case b: VarBinder =>
      val tpe = context.valueTypeOption(b).getOrElse { b.tpe.get }
      pp"var ${b.name}: ${tpe}"

    case TypeAlias(name, tparams, tpe) =>
      val tps = show(tparams)
      "type" <+> name.toString <> tps <+> "=" <+> pp"$tpe"

    case EffectAlias(name, tparams, eff) =>
      val tps = show(tparams)
      "effect" <+> name.toString <> tps <+> "=" <+> eff.toString

    case DataType(name, tparams, ctors) =>
      val tps = show(tparams)
      val ctrs = ctors map { ctor =>
        format("def", ctor, ctor.annotatedResult, ctor.annotatedEffects)
      }
      "type" <+> name.toString <> tps <+> braces(nest(line <> vsep(ctrs)) <> line)

    case Record(name, tparams, ctor) =>
      val tps = show(tparams)
      val ctrs = format("def", ctor, ctor.annotatedResult, ctor.annotatedEffects)
      "type" <+> name.toString <> tps <+> braces(nest(line <> ctrs) <> line)

    case f: ExternFunction =>
      format("extern def", f, f.annotatedResult, f.annotatedEffects)

    case ExternType(name, tparams) =>
      val tps = show(tparams)
      pp"extern type ${name}$tps"

    case ExternInterface(name, tparams) =>
      pp"extern interface ${name}${show(tparams)}"

    case ExternResource(name, tpe) =>
      pp"extern resource ${name}: ${tpe}"

    case c: Callable =>
      val tpe = context.functionTypeOption(c)
      format("def", c, tpe.map { _.result }, tpe.map { _.effects })

    case d: DefBinder =>
      val tpe = context.blockTypeOption(d).getOrElse { d.tpe.get }
      pp"def ${ d.name }: ${ tpe }"
  }

  def format(kw: String, f: Callable, result: Option[List[ValueType]], effects: Option[Effects]): Doc = {
    val tps = if (f.tparams.isEmpty) "" else s"[${f.tparams.mkString(", ")}]"

    val valueParams = f.vparams.map { p => pp"${p.name}: ${p.tpe.get}" }.mkString(", ")
    val vps = if valueParams.isEmpty then "" else s"($valueParams)"
    val bps = f.bparams.map { b => pp"{ ${b.name}: ${b.tpe} }" }.mkString("")

    val ps = if (vps.isEmpty && bps.isEmpty) "()" else s"$vps$bps"

    val returnType = for {
      tpe <- result
      eff <- effects
    } yield pp": $tpe / $eff"

    s"$kw ${f.name}$tps$ps${returnType.getOrElse("")}"
  }
}
