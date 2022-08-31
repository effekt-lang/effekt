package effekt
package lifted

import effekt.context.Context
import effekt.symbols.{ Name, Symbol, TermSymbol, ValueSymbol, BlockSymbol, Interface, InterfaceType, Operation, Type, ValueType, FunctionType, BlockType, TrackedParam }

sealed trait Tree
/**
 * A module declaration, the path should be an Effekt include path, not a system dependent file path
 */
case class ModuleDecl(path: String, imports: List[String], defs: Stmt, exports: List[Symbol]) extends Tree

/**
 * Fine-grain CBV: Arguments can be either expressions or blocks
 */
sealed trait Argument extends Tree

/**
 * Expressions
 */
sealed trait Expr extends Argument
case class ValueVar(id: ValueSymbol) extends Expr

sealed trait Literal[T] extends Expr {
  def value: T
}
case class UnitLit() extends Literal[Unit] { def value = () }
case class IntLit(value: Int) extends Literal[Int]
case class BooleanLit(value: Boolean) extends Literal[Boolean]
case class DoubleLit(value: Double) extends Literal[Double]
case class StringLit(value: String) extends Literal[String]

case class PureApp(b: Block, targs: List[Type], args: List[Argument]) extends Expr
case class Select(target: Expr, field: Symbol) extends Expr
case class Closure(b: Block) extends Expr
case class Run(s: Stmt) extends Expr

/**
 * Blocks
 */
sealed trait Param extends Tree { def id: Symbol }
case class ValueParam(id: ValueSymbol, tpe: ValueType) extends Param
case class BlockParam(id: BlockSymbol, tpe: BlockType) extends Param
case class EvidenceParam(id: EvidenceSymbol) extends Param

sealed trait Block extends Argument
case class BlockVar(id: BlockSymbol) extends Block

// TODO add type params here
case class BlockLit(params: List[Param], body: Stmt) extends Block
case class Member(b: Block, field: TermSymbol) extends Block
case class Extern(params: List[Param], body: String) extends Block
case class Unbox(e: Expr) extends Block
case class New(impl: Handler) extends Block

/**
 * Statements
 */
sealed trait Stmt extends Tree
case class Def(id: BlockSymbol, tpe: BlockType, block: Block, rest: Stmt) extends Stmt
case class Val(id: ValueSymbol, tpe: ValueType, binding: Stmt, body: Stmt) extends Stmt
case class Let(id: ValueSymbol, tpe: ValueType, binding: Expr, body: Stmt) extends Stmt
case class Data(id: Symbol, ctors: List[Symbol], rest: Stmt) extends Stmt
case class Record(id: Symbol, fields: List[Symbol], rest: Stmt) extends Stmt

case class App(b: Block, targs: List[Type], args: List[Argument]) extends Stmt

case class If(cond: Expr, thn: Stmt, els: Stmt) extends Stmt
case class While(cond: Stmt, body: Stmt) extends Stmt
case class Return(e: Expr) extends Stmt
case class Match(scrutinee: Expr, clauses: List[(Pattern, BlockLit)]) extends Stmt

sealed trait Pattern extends Tree
case class IgnorePattern() extends Pattern
case class AnyPattern() extends Pattern
case class TagPattern(tag: Symbol, patterns: List[Pattern]) extends Pattern
case class LiteralPattern[T](l: Literal[T]) extends Pattern

case class Include(contents: String, rest: Stmt) extends Stmt

case object Hole extends Stmt

case class State(id: Symbol, init: Expr, region: Symbol, body: Stmt) extends Stmt
case class Handle(body: Block, handler: List[Handler]) extends Stmt
// TODO change to Map
case class Handler(id: Interface, clauses: List[(Operation, BlockLit)]) extends Tree

case class Region(body: Block) extends Stmt

/**
 * Evidence for lifts
 */
case class Evidence(scopes: List[EvidenceSymbol]) extends Argument

def Here() = Evidence(Nil)

class EvidenceSymbol() extends Symbol { val name = Name.local(s"ev${id}") }
