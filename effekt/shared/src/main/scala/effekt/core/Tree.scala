package effekt
package core

import effekt.context.Context
import effekt.symbols.{ Name, Symbol, TermSymbol, ValueSymbol, BlockSymbol, Type, InterfaceType, ValueType, FunctionType, BlockType, Operation }

sealed trait Tree extends Product {
  def inheritPosition(from: source.Tree)(implicit C: Context): this.type = {
    C.positions.dupPos(from, this);
    this
  }
}

/**
 * A module declaration, the path should be an Effekt include path, not a system dependent file path
 */
case class ModuleDecl(path: String, imports: List[String], defs: Stmt) extends Tree

/**
 * Expressions
 */
sealed trait Expr extends Tree
case class ValueVar(id: ValueSymbol) extends Expr

sealed trait Literal[T] extends Expr {
  def value: T
}
case class UnitLit() extends Literal[Unit] { def value = () }
case class IntLit(value: Int) extends Literal[Int]
case class BooleanLit(value: Boolean) extends Literal[Boolean]
case class DoubleLit(value: Double) extends Literal[Double]
case class StringLit(value: String) extends Literal[String]

case class PureApp(b: Block, targs: List[Type], vargs: List[Expr], bargs: List[Block]) extends Expr
case class Box(b: Block) extends Expr

/**
 * Blocks
 */
sealed trait Param extends Tree { def id: TermSymbol }
case class ValueParam(id: ValueSymbol, tpe: ValueType) extends Param
case class BlockParam(id: BlockSymbol, tpe: BlockType) extends Param

sealed trait Block extends Tree
case class BlockVar(id: BlockSymbol) extends Block

// TODO add type params here
case class BlockLit(vparams: List[ValueParam], bparams: List[BlockParam], body: Stmt) extends Block
// TODO use resolved selector here
case class Select(b: Block, selector: String) extends Block
case class Extern(pure: Boolean, params: List[Param], body: String) extends Block
case class Unbox(e: Expr) extends Block

case class New(tpe: InterfaceType, members: List[(Operation, BlockLit)]) extends Block

/**
 * Statements
 */
sealed trait Stmt extends Tree
case class Def(id: BlockSymbol, tpe: BlockType, block: Block, rest: Stmt) extends Stmt
case class Val(id: ValueSymbol, tpe: ValueType, binding: Stmt, body: Stmt) extends Stmt
case class Data(id: Symbol, ctors: List[Symbol], rest: Stmt) extends Stmt
case class Interface(id: Symbol, ops: List[Symbol], rest: Stmt) extends Stmt
case class App(b: Block, targs: List[Type], vargs: List[Expr], bargs: List[Block]) extends Stmt

case class If(cond: Expr, thn: Stmt, els: Stmt) extends Stmt
case class While(cond: Stmt, body: Stmt) extends Stmt
case class Ret(e: Expr) extends Stmt
case class Exports(path: String, exports: List[Symbol]) extends Stmt
case class Match(scrutinee: Expr, clauses: List[(Pattern, BlockLit)]) extends Stmt

sealed trait Pattern extends Tree
case class IgnorePattern() extends Pattern
case class AnyPattern() extends Pattern
case class TagPattern(tag: Symbol, patterns: List[Pattern]) extends Pattern
case class LiteralPattern[T](l: Literal[T]) extends Pattern

case class Include(contents: String, rest: Stmt) extends Stmt

case object Hole extends Stmt

case class State(init: Stmt, region: Option[Symbol], body: Block) extends Stmt

case class Region(body: Block) extends Stmt
case class Handle(body: Block, handler: List[Handler]) extends Stmt
//// TODO change to Map
case class Handler(tpe: InterfaceType, clauses: List[(Operation, BlockLit)]) extends Tree
