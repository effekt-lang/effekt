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

    //    case e @ UserEffect(name, tparams, List(op)) =>
    //      format("effect", op, op.ret.get)
    //
    //    case e @ UserEffect(name, tparams, ops) =>
    //      val tps = if (tparams.isEmpty) "" else s"[${tparams.mkString(", ")}]"
    //      val effs = ops.map { op => format("def", op, op.ret.get) }
    //      "effect" <+> name.toString <> tps <+> braces(nest(line <> vsep(effs)) <> line)

    case b @ ValBinder(name, tps, decl) =>
      val tpe = context.valueTypeOption(b).getOrElse { b.tpe.get }
      s"val ${name}: ${tpe}"

    case b: VarBinder =>
      val tpe = context.valueTypeOption(b).getOrElse { b.tpe.get }
      s"var ${b.name}: ${tpe}"

    case DataType(name, tparams, ctors) =>
      val tps = if (tparams.isEmpty) "" else s"[${tparams.mkString(", ")}]"
      val ctrs = ctors map { ctor =>
        format("def", ctor, ctor.ret.get)
      }
      "type" <+> name.toString <> tps <+> braces(nest(line <> vsep(ctrs)) <> line)

    case f: BuiltinFunction =>
      format("extern def", f, f.ret.get)

    //    case BuiltinEffect(name, tparams) =>
    //      val tps = if (tparams.isEmpty) "" else s"[${tparams.mkString(", ")}]"
    //      s"extern effect ${name}$tps"

    case BuiltinType(name, tparams) =>
      val tps = if (tparams.isEmpty) "" else s"[${tparams.mkString(", ")}]"
      s"extern type ${name}$tps"

    case c: Fun =>
      format("def", c, context.functionTypeOption(c).map(_.ret))
  }
  def format(kw: String, f: Fun, ret: ValueType): Doc = format(kw, f, Some(ret))
  def format(kw: String, f: Fun, ret: Option[ValueType]): Doc = {
    val tps = if (f.tparams.isEmpty) "" else s"[${f.tparams.mkString(", ")}]"
    val vps = { f.vparams.map { p => s"${p.name}: ${p.tpe}" }.mkString(", ") }
    val bps = f.bparams.map { b => s"{ ${b.name}: ${b.tpe} }" }.mkString("")

    s"$kw ${f.name}$tps($vps)$bps${ret.map { tpe => s": $tpe" }.getOrElse("")}"
  }
}
