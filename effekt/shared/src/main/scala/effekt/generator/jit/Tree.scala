package effekt
package generator
package jit

import effekt.context.Context
import effekt.symbols.{BlockSymbol, Symbol, ValueSymbol}

type Id = core.Id | String
case class Clause(params: List[LhsOperand], body: Term) extends Tree

sealed trait Tree
sealed trait Term

// Basics
case class Var(name: Id, tpe: Type) extends Term
type LhsOperand = Var
case class Abs(params: List[LhsOperand], body: Term) extends Term
case class App(fn: Term, args: List[Term]) extends Term
case class Seq(ts: List[Term]) extends Term

case class Let(defs: List[Definition], body: Term) extends Term
case class LetRec(defs: List[Definition], body: Term) extends Term
case class IfZero(cond: Term, thn: Term, els: Term) extends Term
case class The(tpe: Type, body: Term) extends Term

// Data
case class Construct(tpe_tag: Id, tag: Id, args: List[Term]) extends Term
case class Project(scrutinee: Term, tpe_tag: Id, tag: Id, field: Int) extends Term
case class Match(scrutinee: Term, tpe_tag: Id, clauses: List[(Id, Clause)], default_clause: Clause) extends Term

// Codata
case class New(ifce_tag: Id, methods: List[(Id, Clause)]) extends Term
case class Invoke(receiver: Term, ifce_tag: Id, method: Id, args: List[Term]) extends Term

// References
case class LetRef(ref: LhsOperand, region: Term, binding: Term, body: Term) extends Term
case class Load(ref: Term) extends Term
case class Store(ref: Term, value: Term) extends Term

// Control operators
case class FreshLabel() extends Term
case class Reset(label: Term, region: LhsOperand, body: Term, ret: Clause) extends Term
case class Shift(label: Term, n: Term, k: LhsOperand, body: Term, retTpe: Type) extends Term
case class Control(label: Term, n: Term, k: LhsOperand, body: Term, retTpe: Type) extends Term
case class Resume(cont: Term, args: List[Term]) extends Term
/** Like [[Resume]], but evaluates [[body]] with the continuation already pushed.
 * I.e., prompts in cont are visible from [[body]] */
case class Resumed(cont: Term, body: Term) extends Term

// Effects
enum HandlerType {
  case Shallow, Deep
}
case class Handle(tpe: HandlerType, tag: Id, handlers: List[(Id, Clause)], ret: Option[Clause], body: Term) extends Term
case class Op(tag: Id, op: Id, args: List[Term], k: Clause, rtpe: Type) extends Term
case class DHandle(tpe: HandlerType, tag: Term, handlers: List[(Id, Clause)], ret: Option[Clause], body: Term) extends Term
case class DOp(tag: Term, op: Id, args: List[Term], k: Clause, rtpe: Type) extends Term

// Primitives
case class Primitive(name: String, args: List[Term], returns: List[LhsOperand], rest: Term) extends Term

enum Literal extends Term {
  case Int(value: scala.Int)
  case Double(value: scala.Double)
  case String(value: java.lang.String)
  case NullLabel
  case Unit

  def tpe: Base = this match {
    case Int(_) => Base.Int
    case Double(_) => Base.Double
    case String(_) => Base.String
    case Unit => Base.Unit
    case NullLabel => Base.Label
  }
}


// Top-level
case class Definition(name: LhsOperand, binding: Term)
case class Program(definitions: List[Definition], main: Term) extends Tree

