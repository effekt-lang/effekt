package effekt.symbols

import effekt.context.Context
import effekt.source.Maybe
import effekt.symbols.TypePrinter.show
import kiama.output.ParenPrettyPrinter

import scala.language.implicitConversions

object SignaturePrinter extends ParenPrettyPrinter {

  override val defaultIndent = 2

  def apply(t: Symbol)(implicit context: Context): String =
    pretty(toDoc(t, context)).layout

  def toDoc(t: Symbol, context: Context): Doc = t match {
    case e @ Interface(name, tparams, List(op), _) if op.name.name == name.name =>
      format("effect", op, op.annotatedResult, op.annotatedEffects)

    case e @ Interface(name, tparams, ops, _) =>
      val tps = show(tparams)
      "interface" <+> name.toString <> tps

    case o @ Operation(name, tparams, vparams, bparams, resultType, effects, interface, decl) =>
      val tps = show(tparams)
      val vps = vparams.map { p => pp"${p.name.name}: ${p.tpe.get}" }.mkString(", ")
      val bps = bparams.map { b => pp"{ ${b.name.name}: ${b.tpe.get} }" }.mkString("")
      val ps = if (vps.isEmpty && bps.isEmpty) "()" else s"($vps$bps)"
      val returnType = pp": ${resultType} / ${effects}"
      pp"effect ${name.name}$tps$ps$returnType"

    case b @ ValBinder(name, tps, decl) =>
      val tpe = context.valueTypeOption(b).getOrElse { b.tpe.get }
      pp"val ${name.name}: ${tpe}"

    case p @ ValueParam(name, t, _) =>
      val tpe = context.valueTypeOption(p).getOrElse { t.get }
      pp"${name.name}: ${tpe}"

    case p @ BlockParam(name, t, _) =>
      val tpe = context.blockTypeOption(p).getOrElse { t.get }
      pp"{ ${name.name}: ${tpe} }"

    case b: VarBinder =>
      val tpe = context.valueTypeOption(b).getOrElse { b.tpe.get }
      pp"var ${b.name.name}: ${tpe}"

    case TypeAlias(name, tparams, tpe, _) =>
      val tps = show(tparams)
      "type" <+> name.name <> tps

    case EffectAlias(name, tparams, eff, _) =>
      val tps = show(tparams)
      "effect" <+> name.name <> tps

    case DataType(name, tparams, ctors, _) =>
      val tps = show(tparams)
      "type" <+> name.name <> tps

    case Record(name, tparams, ctor, _) =>
      val tps = show(tparams)
      "type" <+> name.name <> tps

    case f: ExternFunction =>
      format("extern def", f, f.annotatedResult, f.annotatedEffects)

    case ExternType(name, tparams, _) =>
      val tps = show(tparams)
      pp"extern type ${name.name}$tps"

    case ExternInterface(name, tparams, _) =>
      pp"extern interface ${name.name}${show(tparams)}"

    case ExternResource(name, tpe, _) =>
      pp"extern resource ${name.name}: ${tpe}"

    case c: Callable =>
      val tpe = context.functionTypeOption(c)
      format("def", c, tpe.map { _.result }, tpe.map { _.effects })

    case d: DefBinder =>
      val tpe = context.blockTypeOption(d).getOrElse { d.tpe.get }
      pp"def ${ d.name.name }: ${ tpe }"
  }

  def format(kw: String, f: Callable, result: Option[ValueType], effects: Option[Effects]): Doc = {
    val tps = if (f.tparams.isEmpty) "" else s"[${f.tparams.mkString(", ")}]"

    val valueParams = f.vparams.map { p => pp"${p.name.name}: ${p.tpe.get}" }.mkString(", ")
    val vps = if valueParams.isEmpty then "" else s"($valueParams)"
    val bps = f.bparams.map { b => pp"{ ${b.name.name}: ${b.tpe.get} }" }.mkString("")

    val ps = if (vps.isEmpty && bps.isEmpty) "()" else s"$vps$bps"

    val returnType = for {
      tpe <- result
      eff <- effects
    } yield pp": $tpe / $eff"

    s"$kw ${f.name.name}$tps$ps${returnType.getOrElse("")}"
  }
}
