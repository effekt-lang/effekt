package effekt
package machine

import effekt.context.Context
import effekt.symbols.{ Symbol, ValueSymbol, BlockSymbol }

sealed trait Tree extends Product {
  def inheritPosition(from: source.Tree)(implicit C: Context): this.type = {
    C.positions.dupPos(from, this);
    this
  }
}

/**
 * A module declaration
 */
case class ModuleDecl(decls: List[Decl], prog: Stmt) extends Tree

/**
 * Toplevel declarations
 */
sealed trait Decl extends Tree

case class DefPrim(typ: Type, id: BlockSymbol, params: List[Param], body: String) extends Decl
case class Include(contents: String) extends Decl

/**
 * Statements
 */
sealed trait Stmt extends Tree
// Instructions
// TODO make type of id part of let
case class Let(id: Symbol, expr: Expr, rest: Stmt) extends Stmt
case class Def(id: BlockSymbol, block: BlockLit, rest: Stmt) extends Stmt
case class PushFrame(cntType: List[Type], block: Block, args: List[Value], rest: Stmt) extends Stmt
case class PushStack(stack: Arg, rest: Stmt) extends Stmt
case class PopStack(id: BlockSymbol, rest: Stmt) extends Stmt
case class CopyStack(id: BlockSymbol, stack: Arg, rest: Stmt) extends Stmt
case class EraseStack(stack: Arg, rest: Stmt) extends Stmt
// Terminators
case class Ret(values: List[Arg]) extends Stmt
case class Jump(block: Block, args: List[Arg]) extends Stmt
case class If(cond: Arg, thenBlock: Block, thenArgs: List[Value], elseBlock: Block, elseArgs: List[Value]) extends Stmt
case class Match(scrutinee: Arg, variant: Int, thenBlock: Block, thenArgs: List[Value], elseBlock: Block, elseArgs: List[Value]) extends Stmt
case class Panic() extends Stmt

/**
 * Arguments
 */
sealed trait Arg extends Tree

/**
 * Expressions
 */
// TODO remove typ method (and type annotation)
sealed trait Expr extends Arg { def typ: Type }

case class AppPrim(typ: Type, id: BlockSymbol, args: List[Arg]) extends Expr
case class NewStack(typ: Type, block: Block, args: List[Value]) extends Expr
case class EviPlus(l: Arg, r: Arg) extends Expr { def typ = Evidence() }
case class EviDecr(l: Arg) extends Expr { def typ = Evidence() }
case class EviIsZero(l: Arg) extends Expr { def typ = PrimBoolean() }
case class Select(typ: Type, target: Arg, field: Int) extends Expr
case class Construct(typ: Type, args: List[Arg]) extends Expr
case class Inject(typ: Type, arg: Arg, variant: Int) extends Expr
case class Reject(typ: Type, arg: Arg, variant: Int) extends Expr

/**
 * Values
 */
sealed trait Value extends Arg

// Refers to values and stacks are values
// TODO Change this symbol back to value symbol.... but what about block parameters?
// TODO swap typ and id
case class Var(typ: Type, id: Symbol) extends Value
case class IntLit(value: Int) extends Value
case class BooleanLit(value: Boolean) extends Value
case class UnitLit() extends Value
case class EviLit(n: Int) extends Value

/**
 * Blocks
 */
sealed trait Block extends Tree

case class BlockLit(params: List[Param], body: Stmt) extends Block
case class BlockVar(id: BlockSymbol) extends Block

/* TODO change `Type` to be something akin to `FrameAtom` (that is a LLVM-compatible value type)
// a frame atom occupies exactly 64 bits
enum FrameAtom:

    // a naked integer of 64 bits
    // (interpretable as any singed/unsigned integer of width <= 64 bits:
    // boolean, uint32, int64, ...)
    case Int()
    case Float()

    // a statically known function pointer
    // (i.e., `malloc` or a user-supplied function's frame copier)
    case StaticFunctionPointer()

    // a dynamic pointer to another stack
    case BoxedStack()

    // a dynamic pointer to a boxed, more complex value object
    // XXX do not scaffold // case BoxedVal(ref: Referencable)

    // a dynamic pointer to a boxed, more complex mutable object
    // XXX do not scaffold // case BoxedVar(ref: Referencable)

// TODO will `FrameLayout` ever be more than just `List[Type]`?
class FrameLayout(val atoms: List[FrameAtom]) {
}
*/

/**
 * Parameters
 */
// TODO swap id and typ
// TODO rename `Param` => ???
case class Param(typ: Type, id: Symbol) extends Tree

/**
 * Types
 */
sealed trait Type extends Tree
case class PrimUnit() extends Type
case class PrimInt() extends Type
case class PrimBoolean() extends Type
case class Record(fieldTypes: List[Type]) extends Type
case class Variant(variantTypes: List[Type]) extends Type
case class Stack(cntType: List[Type]) extends Type
case class Evidence() extends Type
