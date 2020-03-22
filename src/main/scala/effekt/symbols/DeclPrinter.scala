package effekt
package symbols

import effekt.context.CompilerContext
import org.bitbucket.inkytonik.kiama.output.ParenPrettyPrinter

import scala.language.implicitConversions

object DeclPrinter extends ParenPrettyPrinter {

  override val defaultIndent = 2

  import org.bitbucket.inkytonik.kiama.output.PrettyPrinterTypes.Document

  def apply(t: Symbol, context: CompilerContext): String =
    pretty(toDoc(t, context)).layout

  def toDoc(t: Symbol, context: CompilerContext): Doc = t match {
    case e @ UserEffect(name, tparams, List(op)) =>
      format("effect", op, op.ret.get)

    case b @ ValBinder(name, tps, decl) =>
      val tpe = context.valueTypeOrDefault(b, b.tpe.get)
      s"val ${name}: ${tpe}"

    case b: VarBinder =>
      val tpe = context.valueTypeOrDefault(b, b.tpe.get)
      s"var ${b.name}: ${tpe}"

    case DataType(name, tparams, ctors) =>
      val tps  = if (tparams.isEmpty) "" else s"[${ tparams.mkString(", ") }]"
      val ctrs = ctors map { ctor =>
        format("def", ctor, ctor.ret.get)
      }
      "type" <+> name.toString <> tps <+> braces(nest(line <> vsep(ctrs)) <> line)

    case f: BuiltinFunction =>
      format("extern def", f, f.ret.get)

    case BuiltinEffect(name, tparams) =>
      val tps = if (tparams.isEmpty) "" else s"[${ tparams.mkString(", ") }]"
      s"extern effect ${name}$tps"

    case BuiltinType(name, tparams) =>
      val tps = if (tparams.isEmpty) "" else s"[${ tparams.mkString(", ") }]"
      s"extern type ${name}$tps"

    case c: Fun =>
      val tpe = context.blockType(c)
      format("def", c, tpe.ret)
  }

  def format(kw: String, f: Fun, ret: Effectful): Doc = {
    val tps = if (f.tparams.isEmpty) "" else s"[${ f.tparams.mkString(", ") }]"
    val ps  = f.params.map {
      case List(b: BlockParam) => s"{ ${b.name}: ${b.tpe} }"
      case l: List[ValueParam] =>
        val vps = l.map { p => s"${p.name}: ${p.tpe.get}" }.mkString(", ")
        s"($vps)"
    }.mkString
    s"$kw ${f.name}$tps$ps: ${ret}"
  }
}