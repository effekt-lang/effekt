package effekt
package generator
package hvm

import scala.language.implicitConversions
import scala.collection.mutable.{Map => MutableMap}

/**
 * This file defines the syntax of HVM as it is the image of our translation.
 */
 
case class Rule(pats: List[Pattern], body: Term)

//case class MatchRule(constructor: Option[String], body: Term)

case class Definition(name: String, rules: List[Rule], builtin: Boolean)

enum Verbatim {
  case Def(name: String, params: List[Pattern], body: String)//todo besserer Name
  case Include(contents: String)
}

type Adts = MutableMap[Name, Adt]

type Constructors = MutableMap[String, String]
case class Adt(

  ctrs: MutableMap[Name, List[String]],

  builtin: Boolean
)

enum AdtEncoding {
  case Scott
  case TaggedScott

  def default: AdtEncoding = TaggedScott
}

// Define the Book class
case class Book(
  // The function definitions
  defs: MutableMap[Name, Definition],
  // TODO
  externs: List[Verbatim],
  // The algebraic datatypes defined by the program
  adts: Adts,
  // To which type does each constructor belong to
  ctrs: Constructors,
  // A custom or default "main" entrypoint
  entrypoint: Option[Name]
)

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
   case Lam(tag: Tag, nam: Option[String], bod: Term)
   case Var(nam: String)
   //case Chn(tag: Tag, nam: Option[String], bod: Term)
   case Lnk(nam: String)
   case Let(pat: Pattern, value: Term, nxt: Term)
   case Bnd(fun: String, ask: Pattern, value: Term, nxt: Term)
   case Use(name: Option[String], value: Term, nxt: Term)
   case App(tag: Tag, fun: Term, arg: Term)
   //case Tup(els: List[Term])
   //case Dup(tag: Tag, bnd: List[Option[String]], value: Term, nxt: Term)
   //case Sup(tag: Tag, els: List[Term])
   case Fan(fan: FanKind, tag: Tag, els: List[Term])
   case Num(value: Long)
   case Str(value: String)
   case Lst(els: List[Term])
   case Opx(op: Op, fst: Term, snd: Term)
   case Mat(args: List[Term], rules: List[Rule])//patternmatching with adts
   //case NewMAtch(arg: Term, arms: MatchRule)
   case Swt(args: List[Term], rules: List[Rule])//pattern matching with numbers
   case Ref(nam: String)
   case Era
   case Err
}
export Term.*

enum FanKind {
   case Tup
   case Dup
}
export FanKind.*

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
