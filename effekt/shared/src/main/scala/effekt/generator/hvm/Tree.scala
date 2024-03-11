package effekt
package generator
package hvm

import scala.language.implicitConversions

/**
 * This file defines the syntax of HVM as it is the image of our translation.
 */

 enum Term {
    case Var(name: String)
    case Dup(nam0: String, nam1: String, expr: List[Term],body: List[Term])
    case Sup(val0: Term, val1: Term)
    case Let(name: String, expr: Term, body: Term)
    case Lam(name: String, body: Term)
    case App(func: Term, argm: Term)
    case Ctr(name: String, args: List[Term])
    case U6O(numb: Long)
    case F6O(numb: Long)
    case Op2(oper: Oper, val0: Term, val1: Term)
 }
 export Term.*

 enum Oper {
    case Add
    case Sub
    case Mul
    case Div
    case Mod
    case And
    case Or
    case Xor
    case Shl
    case Shr
    case Lte
    case Ltn
    case Eql
    case Gte
    case Gtn
    case Neq
 }
  export Oper.*