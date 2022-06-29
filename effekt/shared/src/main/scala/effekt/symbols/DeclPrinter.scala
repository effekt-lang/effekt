package effekt
package symbols

import effekt.context.Context
import kiama.output.ParenPrettyPrinter

import scala.language.implicitConversions

object DeclPrinter extends ParenPrettyPrinter {

  override val defaultIndent = 2

  def apply(t: Symbol)(implicit context: Context): String =
    pretty(toDoc(t, context)).layout

  def toDoc(t: Symbol, context: Context): Doc = t match {

    case e @ Interface(name, tparams, List(op)) =>
      format("effect", op, op.annotatedResult, op.annotatedEffects)

    case e @ Interface(name, tparams, ops) =>
      val tps = if (tparams.isEmpty) "" else s"[${tparams.mkString(", ")}]"
      val effs = ops.map { op => format("def", op, op.annotatedResult, op.annotatedEffects) }
      "effect" <+> name.toString <> tps <+> braces(nest(line <> vsep(effs)) <> line)

    case b @ ValBinder(name, tps, decl) =>
      val tpe = context.valueTypeOption(b).getOrElse { b.tpe.get }
      pp"val ${name}: ${tpe}"

    case b: VarBinder =>
      val tpe = context.valueTypeOption(b).getOrElse { b.tpe.get }
      pp"var ${b.name}: ${tpe}"

    case TypeAlias(name, tparams, tpe) =>
      val tps = if (tparams.isEmpty) "" else s"[${tparams.mkString(", ")}]"
      "type" <+> name.toString <> tps <+> "=" <+> tpe.toString

    case EffectAlias(name, tparams, eff) =>
      val tps = if (tparams.isEmpty) "" else s"[${tparams.mkString(", ")}]"
      "effect" <+> name.toString <> tps <+> "=" <+> eff.toString

    case DataType(name, tparams, ctors) =>
      val tps = if (tparams.isEmpty) "" else s"[${tparams.mkString(", ")}]"
      val ctrs = ctors map { ctor =>
        format("def", ctor, ctor.annotatedResult, ctor.annotatedEffects)
      }
      "type" <+> name.toString <> tps <+> braces(nest(line <> vsep(ctrs)) <> line)

    case f: BuiltinFunction =>
      format("extern def", f, f.annotatedResult, f.annotatedEffects)

    case BuiltinEffect(name, tparams) =>
      val tps = if (tparams.isEmpty) "" else s"[${tparams.mkString(", ")}]"
      s"extern effect ${name}$tps"

    case BuiltinType(name, tparams) =>
      val tps = if (tparams.isEmpty) "" else s"[${tparams.mkString(", ")}]"
      s"extern type ${name}$tps"

    case c: Fun =>
      val tpe = context.functionTypeOption(c)
      format("def", c, tpe.map { _.result }, tpe.map { _.effects })
  }

  def format(kw: String, f: Fun, result: Option[ValueType], effects: Option[Effects]): Doc = {
    val tps = if (f.tparams.isEmpty) "" else s"[${f.tparams.mkString(", ")}]"

    val valueParams = f.vparams.map { p => s"${p.name}: ${p.tpe.get}" }.mkString(", ")
    val vps = if valueParams.isEmpty then "" else s"($valueParams)"
    val bps = f.bparams.map { b => s"{ ${b.name}: ${b.tpe} }" }.mkString("")

    val returnType = for {
      tpe <- result
      eff <- effects
    } yield pp": $tpe / $eff"

    s"$kw ${f.name}$tps$vps$bps${returnType.getOrElse("")}"
  }
}
