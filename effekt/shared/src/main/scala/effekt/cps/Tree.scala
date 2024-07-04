package effekt
package cps

import effekt.core.CoreParsers.definition
import effekt.symbols.{ Constructor, Name, Symbol}


export effekt.core.Id

case class ModuleDecl(
  path: String,
  includes: List[String],
  decls: List[Declaration],
  externs: List[Extern],
  definitions: List[Definition],
  exports: List[Symbol]
)

/**
 * Toplevel data and interface declarations
 */
enum Declaration {
  def id: Id

  case Data(id: Id, constructors: List[Constructor])
  case InterfaceDecl(id: Id, properties: List[Property])
}
export Declaration.*

case class Constructor(id: Id, fields: List[Id])
case class Property(id: Id, args: List[Id])

/**
 * FFI external definitions
 */
enum Extern {
  // WARNING: builtins do not take evidence. If they are passed as function argument, they need to be eta-expanded.
  //   (however, if they _would_ take evidence, we could model mutable state with this)
  // TODO revisit
  case Def(id: Id, params: List[Id], body: Template[Expr])
  case Include(contents: String)
}



enum Definition {
  case Function(name: Id, params: List[Id], cont: Id, body: Term)
  case Let(id: Id, binding: Expr)
}


enum Expr{
  case Var(name: Id)
  case Lit(n: Int)
  case UnitLit()
  case PureApp(b: Expr, args: List[Expr])

  case Make(data: Id, tag: Id, vargs: List[Expr])
  case Select(target: Expr, field: Id)
  case Box(b: Expr)

  case Run(t: Term)
  
  case BlockLit(params: List[Id], body: Term)
  case Member(Interface: Expr, field: Id, tpe: Interface)
  case New(impl: Implementation)
  case Evidence(lifts: List[Lift]) // expr als case Nil=> here, List(_) => plus
}
export Expr.*


enum Term {
  case Fun(name: Id, params: List[Id], cont: Id, body: Term)
  case LetCont(cont: Id, param: Id, body: Term, rest: Term)
  case Let(name: Id, expr: Expr, rest: Term)
  case AppCont(cont: Id, arg: Expr)
  case App(func: Expr, arg: List[Expr], cont: Id)
  case Scope(definitions: List[Definition], body: Term)
  case If(cond: Expr, thn: Term, els: Term)
  case Match(scrutinee: Expr, clauses: List[(Id, BlockLit)], default: Option[Term])
  case Reset(evidence: Id, body: Term)
  case Shift(evidence: Id, cont: Id, body: Term)
} 
export Term.*

/**
 * An instance of an interface, concretely implementing the operations.
 *
 * Used to represent handlers / capabilities, and objects / modules.
 */
case class Implementation(interface: Interface, operations: List[Operation]) {
  val tpe = interface
}

/**
 * Implementation of a method / effect operation.
 */
case class Operation(name: Id, implementation: Expr.BlockLit)

case class Interface(name: Id, args: List[Id])

enum Lift {
  case Var(ev: Id)
  case Try()
  case Reg() // used for local mutable state AND region based state
} 


def Here() = Evidence(Nil)


