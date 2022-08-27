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
sealed trait Expr extends Tree with Argument
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
sealed trait Param extends Tree { def id: TermSymbol }
case class ValueParam(id: ValueSymbol, tpe: ValueType) extends Param
case class BlockParam(id: BlockSymbol, tpe: BlockType) extends Param

sealed trait Block extends Tree with Argument
case class BlockVar(id: BlockSymbol) extends Block

// introduced by lift inference only
case class ScopeAbs(scope: Symbol, body: Block) extends Block
case class ScopeApp(b: Block, evidence: Scope) extends Block
case class Lifted(s: Scope, b: Block) extends Block

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
case class Ret(e: Expr) extends Stmt
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
 * Explicit Lifts
 * ---
 * introduced by lift inference only
 * TODO maybe add a separate core language with explicit lifts
 */
sealed trait Scope extends Tree
case class Here() extends Scope
case class Nested(list: List[Scope]) extends Scope
case class ScopeVar(id: Symbol) extends Scope

case class ScopeId() extends Symbol { val name = Name.local(s"ev${id}") }


def freeVariables(stmt: Stmt): Set[Symbol] = stmt match {
  case Def(id, tpe, block, rest) => (freeVariables(block) ++ freeVariables(rest)) -- Set(id)
  case Val(id, tpe, binding, body) => freeVariables(binding) ++ freeVariables(body) -- Set(id)
  case Let(id, tpe, binding, body) => freeVariables(binding) ++ freeVariables(body) -- Set(id)
  case Data(id, ctors, rest) => freeVariables(rest)
  case Record(id, fields, rest) =>freeVariables(rest)
  case App(b, targs, args) => freeVariables(b) ++ args.flatMap(freeVariables)
  case If(cond, thn, els) => freeVariables(cond) ++ freeVariables(thn) ++ freeVariables(els)
  case While(cond, body) => freeVariables(cond) ++ freeVariables(body)
  case Ret(e) => freeVariables(e)
  case Match(scrutinee, clauses) => freeVariables(scrutinee) ++ clauses.flatMap { case (pattern, lit) => freeVariables(lit) }
  case Include(contents, rest) => freeVariables(rest)
  case Hole => Set.empty
  case State(id, init, region, body) => freeVariables(init) ++ freeVariables(body) -- Set(id, region)
  case Handle(body, handlers) => freeVariables(body) ++ handlers.flatMap {
    case Handler(id, clauses) => clauses.flatMap { case (operation, lit) => freeVariables(lit) }
  }
  case Region(body) => freeVariables(body)
}

def freeVariables(expr: Expr): Set[Symbol] = expr match {
  case ValueVar(id) => Set(id)
  case literal: Literal[_] => Set.empty
  case PureApp(b, targs, args) => freeVariables(b) ++ args.flatMap(freeVariables)
  case Select(target, field) => freeVariables(target) // we do not count fields in...
  case Closure(b) => freeVariables(b) // well, well, well...
  case Run(s) => freeVariables(s)
}

def freeVariables(arg: Argument): Set[Symbol] = arg match {
  case expr: Expr => freeVariables(expr)
  case block: Block => freeVariables(block)
}

def freeVariables(block: Block): Set[Symbol] = block match {
  case BlockVar(id) => Set(id)
  case ScopeAbs(scope, body) => freeVariables(body) -- Set(scope)
  case ScopeApp(b, evidence) => freeVariables(b) ++ freeVariables(evidence)
  case Lifted(s, b) => freeVariables(b) ++ freeVariables(s)
  case BlockLit(params, body) =>
    val bound = params.map {
      case ValueParam(id, tpe) => id
      case BlockParam(id, tpe) => id
    }
    freeVariables(body) -- bound
  case Member(b, field) => freeVariables(b)
  case Extern(params, body) => Set.empty
  case Unbox(e) => freeVariables(e) // TODO well, well, well...
  case _ => ??? // TODO (see also e2c5547b32e40697cafaec51f8e3c27ce639055e)
}

def freeVariables(scope: Scope): Set[Symbol] = scope match {
  case Here() => Set.empty
  case Nested(list) => list.flatMap(freeVariables).toSet
  case ScopeVar(id) => Set(id)
}
