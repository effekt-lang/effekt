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

/**
 * Values
 */
sealed trait Value extends Arg

// Refers to values and stack are values
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

/**
 * Parameters
 */
// TODO swap id and typ
case class Param(typ: Type, id: Symbol) extends Tree

/**
 * Types
 */
sealed trait Type extends Tree

case class PrimUnit() extends Type
case class PrimInt() extends Type
case class PrimBoolean() extends Type
case class Record(fieldTypes: List[Type]) extends Type
case class Stack(cntType: List[Type]) extends Type
case class Evidence() extends Type

