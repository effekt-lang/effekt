package effekt
package core
package syntax

type Id = String

sealed trait Tree

case class ModuleDecl(
  path: String,
  includes: List[String],
  declarations: List[Declaration],
  externs: List[Extern],
  definitions: List[Toplevel],
  exports: List[Id]
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

enum FeatureFlag extends Tree {
  case NamedFeatureFlag(id: String)
  case Default
}

/**
 * FFI external definitions
 */
enum Extern extends Tree {
  case Def(id: Id, tparams: List[Id], cparams: List[Id], vparams: List[ValueParam], bparams: List[BlockParam], ret: ValueType, body: ExternBody)
  case Include(featureFlag: FeatureFlag, contents: String)
}
sealed trait ExternBody extends Tree
object ExternBody {
  case class StringExternBody(featureFlag: FeatureFlag, contents: Template[Pure]) extends ExternBody
  case class Unsupported(err: util.messages.EffektError) extends ExternBody
}

enum Toplevel {
  def id: Id

  case Def(id: Id, block: Block)
  case Val(id: Id, tpe: ValueType, binding: core.Stmt)
}


/**
 * Expressions (with potential IO effects)
 *
 * - [[DirectApp]]
 * - [[Pure]]
 */
sealed trait Expr extends Tree

// invariant, block b is {io}.
case class DirectApp(b: Block.BlockVar, targs: List[ValueType], vargs: List[Pure], bargs: List[Block]) extends Expr

enum Pure extends Expr {

  case ValueVar(id: Id)

  case Literal(value: Any)

  case PureApp(b: Block.BlockVar, targs: List[ValueType], vargs: List[Pure])

  case Make(data: ValueType.Data, tag: Id, targs: List[ValueType], vargs: List[Pure])

  case Box(b: Block)
}
export Pure.*

enum Block extends Tree {
  case BlockVar(id: Id)
  case BlockLit(tparams: List[Id], cparams: List[Id], vparams: List[ValueParam], bparams: List[BlockParam], ret: ValueType, body: Stmt)
  case Unbox(pure: Pure)
  case New(impl: Implementation)
}
export Block.*

case class ValueParam(id: Id, tpe: ValueType)
case class BlockParam(id: Id, tpe: BlockType)

enum Stmt extends Tree {
  case Def(id: Id, block: Block, body: Stmt)
  case Let(id: Id, tpe: ValueType, binding: Expr, body: Stmt)
  case Return(expr: Pure)
  case Val(id: Id, tpe: ValueType, binding: Stmt, body: Stmt)
  case App(callee: Block, targs: List[ValueType], vargs: List[Pure], bargs: List[Block])
  case Invoke(callee: Block, method: Id, targs: List[ValueType], vargs: List[Pure], bargs: List[Block])
  case If(cond: Pure, thn: Stmt, els: Stmt)
  case Match(scrutinee: Pure, clauses: List[(Id, BlockLit)], default: Option[Stmt])
  case Region(body: BlockLit)
  case Alloc(id: Id, init: Pure, region: Id, body: Stmt)
  case Var(ref: Id, init: Pure, capture: Id, body: Stmt)
  case Get(id: Id, ref: Id, body: Stmt)
  case Put(ref: Id, value: Pure, body: Stmt)
  case Reset(body: Block.BlockLit)
  case Shift(prompt: BlockVar, body: BlockLit)
  case Resume(k: BlockVar, body: Stmt)
  case Hole()
}
export Stmt.*


case class Implementation(interface: BlockType.Interface, operations: List[Operation]) extends Tree

case class Operation(name: Id, tparams: List[Id], cparams: List[Id], vparams: List[ValueParam], bparams: List[BlockParam], body: Stmt)


/**
 * Types
 */
sealed trait Type

enum ValueType extends Type {
  case Var(name: Id)
  case Data(name: Id, targs: List[ValueType])
  case Boxed(tpe: BlockType, capt: Captures)
}

enum BlockType extends Type {
  case Function(tparams: List[Id], cparams: List[Id], vparams: List[ValueType], bparams: List[BlockType], result: ValueType)
  case Interface(name: Id, targs: List[ValueType])
}
