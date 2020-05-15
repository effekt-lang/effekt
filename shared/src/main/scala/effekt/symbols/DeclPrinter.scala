package effekt
package symbols

import effekt.context.Context
import org.bitbucket.inkytonik.kiama.output.ParenPrettyPrinter

import scala.language.implicitConversions

object DeclPrinter extends ParenPrettyPrinter {

  override val defaultIndent = 2

  def apply(t: Symbol)(implicit context: Context): String =
    pretty(toDoc(t, context)).layout

  def toDoc(t: Symbol, context: Context): Doc = t match {

    case e @ UserEffect(name, tparams, List(op)) =>
      format("effect", op, op.ret.get)

    case e @ UserEffect(name, tparams, ops) =>
      val tps = if (tparams.isEmpty) "" else s"[${tparams.mkString(", ")}]"
      val effs = ops.map { op => format("def", op, op.ret.get) }
      "effect" <+> name.toString <> tps <+> braces(nest(line <> vsep(effs)) <> line)

    case b @ ValBinder(name, tps, decl) =>
      val tpe = context.valueTypeOption(b).getOrElse { b.tpe.get }
      s"val ${name}: ${tpe}"

    case b: VarBinder =>
      val tpe = context.valueTypeOption(b).getOrElse { b.tpe.get }
      s"var ${b.name}: ${tpe}"

    case TypeAlias(name, tparams, tpe) =>
      val tps = if (tparams.isEmpty) "" else s"[${tparams.mkString(", ")}]"
      "type" <+> name.toString <> tps <+> "=" <+> tpe.toString

    case EffectAlias(name, eff) =>
      "effect" <+> name.toString <+> "=" <+> eff.toString

    case DataType(name, tparams, ctors) =>
      val tps = if (tparams.isEmpty) "" else s"[${tparams.mkString(", ")}]"
      val ctrs = ctors map { ctor =>
        format("def", ctor, ctor.ret.get)
      }
      "type" <+> name.toString <> tps <+> braces(nest(line <> vsep(ctrs)) <> line)

    case f: BuiltinFunction =>
      format("extern def", f, f.ret.get)

    case BuiltinEffect(name, tparams) =>
      val tps = if (tparams.isEmpty) "" else s"[${tparams.mkString(", ")}]"
      s"extern effect ${name}$tps"

    case BuiltinType(name, tparams) =>
      val tps = if (tparams.isEmpty) "" else s"[${tparams.mkString(", ")}]"
      s"extern type ${name}$tps"

    case c: Fun =>
      format("def", c, context.blockTypeOption(c).map(_.ret))
  }
  def format(kw: String, f: Fun, ret: Effectful): Doc = format(kw, f, Some(ret))
  def format(kw: String, f: Fun, ret: Option[Effectful]): Doc = {
    val tps = if (f.tparams.isEmpty) "" else s"[${f.tparams.mkString(", ")}]"
    val ps = f.params.map {
      case List(b: BlockParam) => s"{ ${b.name}: ${b.tpe} }"
      case l: List[ValueParam @unchecked] =>
        val vps = l.map { p => s"${p.name}: ${p.tpe.get}" }.mkString(", ")
        s"($vps)"
    }.mkString

    s"$kw ${f.name}$tps$ps${ret.map { tpe => s": $tpe" }}"
  }
}
