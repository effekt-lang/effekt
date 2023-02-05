package effekt
package lifted.typed

import effekt.core.Id

sealed trait Tree
/**
 * A module declaration, the path should be an Effekt include path, not a system dependent file path
 */
case class ModuleDecl(
  path: String,
  imports: List[String],
  decls: List[Declaration],
  externs: List[Extern],
  definitions: List[Definition],
  exports: List[Symbol]
) extends Tree

/**
 * Toplevel data and interface declarations
 */
enum Declaration extends Tree {
  def id: Id

  case Data(id: Id, tparams: List[Id], constructors: List[Constructor])
  case Interface(id: Id, tparams: List[Id], properties: List[Property])
}
export Declaration.*

case class Constructor(id: Id, fields: List[Field]) extends Tree
case class Field(id: Id, tpe: ValueType) extends Tree
case class Property(id: Id, tpe: BlockType) extends Tree

enum Param extends Tree {
  def id: Id

  case ValueParam(id: Id, tpe: ValueType)
  case BlockParam(id: Id, tpe: BlockType)
}
export Param.*

/**
 * FFI external definitions
 */
enum Extern extends Tree {
  // WARNING: builtins do not take evidence. If they are passed as function argument, they need to be eta-expanded.
  //   (however, if they _would_ take evidence, we could model mutable state with this)
  case Def(id: Id, tparams: List[Id], vparams: List[Param.ValueParam], bparams: List[Param.BlockParam], ret: ValueType, body: String)
  case Include(contents: String)
}

enum Definition {
  case Def(id: Id, block: Block)
  case Let(id: Id, binding: Expr)
}

sealed trait Expr extends Tree {
  def tpe: ValueType = ???
}
// might use IO resources, but does not take evidence.
case class DirectApp(b: Block, targs: List[ValueType], vargs: List[Pure], bargs: List[Block])


enum Pure extends Expr {
  case ValueVar(id: Id, annotatedType: ValueType)

  case Literal(value: Any, annotatedType: ValueType)

  // invariant, block b is known to be pure; does not take evidence, does not use IO resources.
  case PureApp(b: Block, targs: List[ValueType], vargs: List[Pure])

  case Select(target: Pure, field: Id, annotatedType: ValueType)

  // WARNING not officially supported, yet
  case Box(b: Block)
}
export Pure.*

enum Block extends Tree {
  case BlockVar(id: Id, annotatedTpe: BlockType)
  case BlockLit(tparams: List[Id], eparams: List[Id], vparams: List[Param.ValueParam], bparams: List[Param.BlockParam], body: Stmt)
  case Member(block: Block, field: Id, annotatedTpe: BlockType)

  // WARNING not officially supported, yet
  case Unbox(pure: Pure)

  case New(impl: Implementation)

  def tpe: BlockType = ???
}
export Block.*

enum Evidence extends Tree {
  case Var(id: Id)
  case Constant(n: Int)
}

/**
 * An instance of an interface, concretely implementing the operations.
 *
 * Used to represent handlers / capabilities, and objects / modules.
 */
case class Implementation(interface: BlockType.Interface, operations: List[Operation]) extends Tree

/**
 * Implementation of a method / effect operation.
 */
case class Operation(name: Id, implementation: Block)

enum Stmt extends Tree {

  case Scope(definitions: List[Definition], body: Stmt)

  // Fine-grain CBV
  case Return(expr: Pure)
  case Val(id: Id, binding: Stmt, body: Stmt)
  case App(callee: Block, targs: List[ValueType], vargs: List[Pure], bargs: List[Block])

  // Local Control Flow
  case If(cond: Pure, thn: Stmt, els: Stmt)
  case Match(scrutinee: Pure, clauses: List[(Id, Block)], default: Option[Stmt])

  // Effects
  case State(id: Id, init: Pure, region: Id, body: Stmt) // TODO maybe rename to Var?
  case Try(body: Block, handlers: List[Implementation])
  case Region(body: Block)

  // e.g. shift(1) { {resume} => stmt }
  case Shift(ev: Evidence, body: Block)

  // Others
  case Hole()

  def tpe: ValueType = ???
}
export Stmt.*

