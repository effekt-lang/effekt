package effekt
package generator
package hvm

import scala.language.implicitConversions

/**
 * This file defines the syntax of HVM as it is the image of our translation.
 */

 //Book, Definition, Adt and Adtencoding missing

 
case class Rule(pats: List[Pattern], body: Term)


enum Tag {
   case Name(name: String) 
   case Numeric(value: Int)
   case Auto 
   case Static
}
export Tag.*


enum Op {
   case Add  
   case Sub 
   case Mul 
   case Div 
   case Mod 
   case Eq 
   case Ne 
   case Lt 
   case Gt 
   case Lte 
   case Gte 
   case And 
   case Or 
   case Xor 
   case Shl 
   case Shr 
}
export Op.*

enum NumCtr {
   case  Num(value: Long)
   case  Succ(value: Long, name: Option[Option[String]])
} 



enum Term {
   case  Lam(tag: Tag, nam: Option[String], bod: Term)
   case  Var(nam: String)
   case  Chn(tag: Tag, nam: Option[String], bod: Term)
   case  Lnk(nam: String)
   case  Let(pat: Pattern, value: Term, nxt: Term)
   case  App(tag: Tag, fun: Term, arg: Term)
   case  Tup(els: List[Term])
   case  Dup(tag: Tag, bnd: List[Option[String]], value: Term, nxt: Term)
   case  Sup(tag: Tag, els: List[Term])
   case  Num(value: Long)
   case  Str(value: String)
   case  Lst(els: List[Term])
   case  Opx(op: Op, fst: Term, snd: Term)
   case  Mat(args: List[Term], rules: List[Rule])
   case  Ref(nam: String)
   case  Era
   case  Err
}
export Term.*


enum Pattern {
   case VarPattern(name: Option[String])
   case Ctr(name: String, patterns: List[Pattern])
   case NumPattern(numCtr: NumCtr)
   case TupPattern(patterns: List[Pattern])
   case LstPattern(patterns: List[Pattern])
   case StrPattern(string: String)
}

export Pattern.*


enum Type {
   case Any
   case TupType(size: Int)
   case NumType
   case NumSuccType(value: Long)
   case AdtType(String: String)
}
