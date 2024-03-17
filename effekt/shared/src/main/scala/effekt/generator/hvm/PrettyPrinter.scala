package effekt
package generator
package hvm

import effekt.util.intercalate
import kiama.output.ParenPrettyPrinter
import kiama.output.PrettyPrinterTypes.Document

import scala.language.implicitConversions
import java.util.Optional
import effekt.util.AnsiHighlight.string
import effekt.source.Named
import effekt.util.AnsiHighlight.string
import effekt.core.normal.valDef

object PrettyPrinter extends ParenPrettyPrinter {

  override val defaultIndent = 2

  val prelude = "#!/usr/local/bin/hvm --script\n\n(import (hvm))\n\n"

  def toDoc(expr: Term): Doc = expr match {
      case Lam(tag, nam, bod)         => toDoc(tag)<+>string("λ")<>toDoc(nam)<+>toDoc(bod)
      case Chn(tag, nam, bod)         => toDoc(tag)<+>string("λ$")<>toDoc(nam)<+>toDoc(bod)
      case Var(nam)                   => string(nam)
      case Lnk(nam)                   => string("$") <> string(nam)
      case Let(pat, value, nxt)       => string("let")<+> toDoc(pat) <+> string("=") <+> toDoc(value)<>string(";")<+>toDoc(nxt)
      case Ref(nam)                   => string(nam)
      case App(tag, fun, arg)         => toDoc(tag)<>parens(toDoc(fun)<+> toDoc(arg))
      case Tup(els)                   => parens(folddoc((els map toDoc), (x, y) => x <> String(",") <+> y))
      case Dup(tag, bnd, value, nxt)  => string("let")<+>toDoc(tag)<>braces(hsep(bnd map toDoc))<+> string("=")<+>toDoc(value)<>string(";")<+> toDoc(nxt)
      case Sup(tag, els)              => toDoc(tag)<>braces(hsep(els map toDoc))
      case Num(value)                 => string(value.toString)
      case Str(value)                 => string(value)
      case Lst(els)                   => brackets(folddoc((els map toDoc), (x, y) => x <> String(",") <+> y))
      case Opx(op, fst, snd)          => parens(toDoc(op) <+> toDoc(fst) <+> toDoc(snd))
      case Mat(args, rules)           => string("match") <+> hsep((args map toDoc), string(",")) <+> brackets(string("wrong"))
      case Era                        => string("*")
      case Err                        => string("<Invalid>")
  }


  def toDoc(op: Op): Doc = op match {
    case Add  => string("+")
    case Sub  => string("-") 
    case Mul  => string("*")
    case Div  => string("/")
    case Mod  => string("%")
    case Eq   => string("==")
    case Ne   => string("!=")
    case Lt   => string("<")
    case Gt   => string(">")
    case Lte  => string("<=")
    case Gte  => string(">=")
    case And  => string("&")
    case Or   => string("|")
    case Xor  => string("^")
    case Shl  => string("<<")
    case Shr  => string(">>")
  }

  def toDoc(tag: Tag): Doc = tag match {
    case Name(name)       => string(name)
    case Numeric(value)   => string(value.toString()) 
    case Auto             => string("")
    case Static           => string("") 
  }

  def toDoc(optional: Option[String]): Doc = optional match {
    case Some(string)  => toDoc(Str(string))
    case None          => string("")
  }

  def toDoc(pat: Pattern) : Doc = pat match {
    case VarPattern(None)           => string("*")
    case VarPattern(Some(name))      => string(name)
    case Ctr(name, patterns)  => parens(string(name)<+>hsep(patterns map toDoc))
    case NumPattern(numCtr)     => toDoc(numCtr)
    case TupPattern(patterns)   => parens(folddoc(patterns map toDoc, (x, y) => x <> String(",") <+> y))
    case LstPattern(patterns)   => brackets(folddoc(patterns map toDoc, (x, y) => x <> String(",") <+> y))
    case StrPattern(value)      => string("""""""")<>string(value)<>string("""""""")
  }

  def toDoc(numCtr: NumCtr) : Doc = numCtr match {
    case NumCtr.Num(value)                     => string(value.toString())
    case NumCtr.Succ(value, None)              => string(value.toString())<>string("+")
    case NumCtr.Succ(value, Some(None))        => string(value.toString())<>string("+*")
    case NumCtr.Succ(value, Some(Some(name)))  => string(value.toString())<>string("+")<>string(name)
  }



}