package effekt
package core

import effekt.symbols.Symbol

sealed trait Tree extends Product


case class ModuleDecl(path: String, imports: List[String], defs: Stmt) extends Tree

/**
 * Fine-grain CBV: Arguments can be either expressions or blocks
 */
sealed trait Argument extends Tree

/**
 * Expressions
 */
sealed trait Expr extends Tree with Argument

case class ValueVar(id: Symbol) extends Expr
case class Assign(id: Symbol, binding: Expr) extends Expr
case class Deref(id: Symbol) extends Expr

sealed trait Literal[T] extends Expr {
  def value: T
}
case class UnitLit() extends Literal[Unit] { def value = () }
case class IntLit(value: Int) extends Literal[Int]
case class BooleanLit(value: Boolean) extends Literal[Boolean]
case class DoubleLit(value: Double) extends Literal[Double]
case class StringLit(value: String) extends Literal[String]

case class PureApp(b: Block, args: List[Argument]) extends Expr

/**
 * Blocks
 */
sealed trait Param extends Tree { def id: Symbol }
case class ValueParam(id: Symbol) extends Param
case class BlockParam(id: Symbol) extends Param

sealed trait Block extends Tree with Argument
case class BlockVar(id: Symbol) extends Block
case class BlockDef(params: List[Param], body: Stmt) extends Block
case class Lift(b: Block) extends Block
case class Extern(params: List[Param], body: String) extends Block

/**
 * Statements
 */
sealed trait Stmt extends Tree
case class Def(id: Symbol, block: Block, rest: Stmt) extends Stmt
case class Val(id: Symbol, binding: Stmt, body: Stmt) extends Stmt
case class Var(id: Symbol, binding: Stmt, body: Stmt) extends Stmt
case class Data(id: Symbol, ctors: List[Symbol], rest: Stmt) extends Stmt
case class App(b: Block, args: List[Argument]) extends Stmt
case class If(cond: Expr, thn: Stmt, els: Stmt) extends Stmt
case class While(cond: Stmt, body: Stmt) extends Stmt
case class Ret(e: Expr) extends Stmt
case class Exports(path: String, exports: List[Symbol]) extends Stmt
case class Handle(body: Block, clauses: List[(Symbol, Block)]) extends Stmt
case class Match(scrutinee: Expr, clauses: List[(Symbol, Block)]) extends Stmt
case class Include(contents: String, rest: Stmt) extends Stmt
