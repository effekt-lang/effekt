package effekt
package core.frontend


type Id = String

// Terms
// -----

case class ModuleDecl(
  declarations: List[Declaration],
  definitions: List[Toplevel]
)

enum Toplevel {
  case Def(id: Id, block: Block)
  case Val(id: Id, binding: Stmt)
}

/**
 * Toplevel data and interface declarations
 */
enum Declaration {
  case Data(id: Id, tparams: List[Id], constructors: List[Constructor])
  case Interface(id: Id, tparams: List[Id], properties: List[Property])
}

case class Constructor(id: Id, tparams: List[Id], fields: List[Field])
case class Field(id: Id, tpe: Type)
case class Property(id: Id, tpe: Type)


case class Implementation(interface: Type.Ref, clauses: List[Operation])
case class Operation(id: Id, impl: Block.BlockLit)

case class Param(id: Id, tpe: Type)

sealed trait Expr
case class DirectApp(b: Id, targs: List[Type], vargs: List[Pure], bargs: List[Block]) extends Expr


// idea for concrete syntax:
//
//
// PureApp:    pure f()
// DirectApp:  io f()
// App:        f()
// Make:       make f()

enum Pure extends Expr {
  case ValueVar(id: Id)
  case Literal(value: Any, annotatedType: core.ValueType)
  case Make(tag: Id, targs: List[Type], vargs: List[Pure])
  case PureApp(b: Id, targs: List[Type], vargs: List[Pure])
  case Box(b: Block)
}

enum Stmt {

  // Definitions
  case Def(id: Id, block: Block, body: Stmt)
  case Let(id: Id, binding: Expr, body: Stmt)

  case Val(id: Id, binding: Stmt, body: Stmt)
  case App(callee: Block, targs: List[Type], vargs: List[Pure], bargs: List[Block])
  case Invoke(callee: Block, method: Id, targs: List[Type], vargs: List[Pure], bargs: List[Block])

  // Fine-grain CBV
  case Return(expr: Pure)

  case Hole()
}

enum Block {
  case BlockVar(id: Id)
  case BlockLit(tparams: List[Id], vparams: List[Param], bparams: List[Param], ret: Option[Type], body: Stmt)
  case Unbox(pure: Pure)
  case New(impl: Implementation)
}



// Types
// -----

type Capture = Id
type Captures = Set[Capture]

enum Type {
  case Ref(id: Id, args: List[Type])
  case Boxed(tpe: Type, capt: Captures)
  case Function(tparams: List[Id], vparams: List[Type], bparams: List[(Option[Id], Type)], result: Type)
}

