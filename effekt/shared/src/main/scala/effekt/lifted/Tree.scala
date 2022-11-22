package effekt
package lifted

import effekt.context.Context
import effekt.symbols.{ BlockSymbol, BlockType, Constructor, FunctionType, Interface, InterfaceType, Name, Operation, Symbol, TermSymbol, TrackedParam, Type, ValueSymbol, ValueType }

sealed trait Tree
/**
 * A module declaration, the path should be an Effekt include path, not a system dependent file path
 */
case class ModuleDecl(
  path: String,
  imports: List[String],
  decls: List[Decl],
  externs: List[Extern],
  defs: Stmt,
  exports: List[Symbol]
) extends Tree

/**
 * Toplevel data and interface declarations
 */
enum Decl {
  case Data(id: Symbol, ctors: List[Symbol])
  case Record(id: Symbol, fields: List[Symbol])
  case Interface(id: Symbol, operations: List[Symbol])
}
export Decl.*

/**
 * FFI external definitions
 */
enum Extern {
  case Def(id: BlockSymbol, tpe: FunctionType, params: List[Param], body: String)
  case Include(contents: String)
}

/**
 * Fine-grain CBV: Arguments can be either pure expressions [[Pure]] or blocks [[Block]]
 */
sealed trait Argument extends Tree

/**
 * Expressions (with potential IO effects)
 */
sealed trait Expr extends Tree

// invariant, block b is {io}.
case class DirectApp(b: Block, targs: List[Type], args: List[Argument]) extends Expr
case class Run(s: Stmt, tpe: ValueType) extends Expr

/**
 * Pure Expressions (no IO effects, or control effects)
 *
 * ----------[[ effekt.lifted.Pure ]]----------
 *
 *   ─ [[ Pure ]]
 *     │─ [[ ValueVar ]]
 *     │─ [[ Literal ]]
 *     │  │─ [[ UnitLit ]]
 *     │  │─ [[ IntLit ]]
 *     │  │─ [[ BooleanLit ]]
 *     │  │─ [[ DoubleLit ]]
 *     │  │─ [[ StringLit ]]
 *     │
 *     │─ [[ PureApp ]]
 *     │─ [[ Select ]]
 *     │─ [[ Box ]]
 *
 * -------------------------------------------
 */
sealed trait Pure extends Expr with Argument
object Pure {
  case class ValueVar(id: ValueSymbol) extends Pure

  enum Literal[T](val value: T) extends Pure {
    case UnitLit() extends Literal(())
    case IntLit(v: Int) extends Literal(v)
    case BooleanLit(v: Boolean) extends Literal(v)
    case DoubleLit(v: Double) extends Literal(v)
    case StringLit(v: String) extends Literal(v)
  }

  // invariant, block b is pure.
  case class PureApp(b: Block, targs: List[Type], args: List[Pure]) extends Pure
  case class Select(target: Pure, field: symbols.Field) extends Pure

  case class Box(b: Block) extends Pure
}
export Pure.*
export Literal.*

/**
 * Blocks
 *
 * ----------[[ effekt.lifted.Block ]]----------
 *
 *   ─ [[ Block ]]
 *     │─ [[ BlockVar ]]
 *     │─ [[ BlockLit ]]
 *     │─ [[ Member ]]
 *     │─ [[ Unbox ]]
 *     │─ [[ New ]]
 *
 * -------------------------------------------
 */
sealed trait Block extends Argument
case class BlockVar(id: BlockSymbol) extends Block
case class BlockLit(params: List[Param], body: Stmt) extends Block
case class Member(b: Block, field: TermSymbol) extends Block
case class Unbox(e: Pure) extends Block
case class New(impl: Implementation) extends Block

sealed trait Param extends Tree { def id: Symbol }
case class ValueParam(id: ValueSymbol, tpe: ValueType) extends Param
case class BlockParam(id: BlockSymbol, tpe: BlockType) extends Param
case class EvidenceParam(id: EvidenceSymbol) extends Param

/**
 * Statements
 *
 * ----------[[ effekt.lifted.Stmt ]]----------
 *
 *   ─ [[ Stmt ]]
 *     │─ [[ Return ]]
 *     │─ [[ Val ]]
 *     │─ [[ Def ]]
 *     │─ [[ Let ]]
 *     │─ [[ App ]]
 *     │─ [[ If ]]
 *     │─ [[ While ]]
 *     │─ [[ Match ]]
 *     │─ [[ State ]]
 *     │─ [[ Try ]]
 *     │─ [[ Region ]]
 *     │─ [[ Hole ]]
 *
 * -------------------------------------------
 */
sealed trait Stmt extends Tree

// Fine-grain CBV
case class Return(e: Pure) extends Stmt
case class Val(id: ValueSymbol, tpe: ValueType, binding: Stmt, body: Stmt) extends Stmt
case class Def(id: BlockSymbol, tpe: BlockType, block: Block, rest: Stmt) extends Stmt
case class Let(id: ValueSymbol, tpe: ValueType, binding: Expr, body: Stmt) extends Stmt
case class App(b: Block, targs: List[Type], args: List[Argument]) extends Stmt

// Local Control Flow
case class If(cond: Pure, thn: Stmt, els: Stmt) extends Stmt
case class While(cond: Stmt, body: Stmt) extends Stmt
case class Match(scrutinee: Pure, clauses: List[(Constructor, BlockLit)], default: Option[Stmt]) extends Stmt

// Effects
case class State(id: Symbol, init: Pure, region: Symbol, body: Stmt) extends Stmt
case class Try(body: Block, answerType: ValueType, handler: List[Implementation]) extends Stmt
case class Region(body: Block) extends Stmt

// Others
case object Hole extends Stmt

/**
 * An instance of an interface, concretely implementing the operations.
 *
 * Used to represent handlers / capabilities, and objects / modules.
 */
case class Implementation(id: symbols.Interface, operations: List[Operation]) extends Tree

/**
 * Implementation of a method / effect operation.
 */
case class Operation(name: symbols.Operation, implementation: BlockLit)


/**
 * Evidence for lifts
 */
case class Evidence(scopes: List[EvidenceSymbol]) extends Argument

def Here() = Evidence(Nil)

class EvidenceSymbol() extends Symbol { val name = Name.local(s"ev${id}") }

def freeVariables(stmt: Stmt): Set[Symbol] = stmt match {
  case Def(id, tpe, block, rest) => (freeVariables(block) ++ freeVariables(rest)) -- Set(id)
  case Val(id, tpe, binding, body) => freeVariables(binding) ++ freeVariables(body) -- Set(id)
  case Let(id, tpe, binding, body) => freeVariables(binding) ++ freeVariables(body) -- Set(id)
  case App(b, targs, args) => freeVariables(b) ++ args.flatMap(freeVariables)
  case If(cond, thn, els) => freeVariables(cond) ++ freeVariables(thn) ++ freeVariables(els)
  case While(cond, body) => freeVariables(cond) ++ freeVariables(body)
  case Return(e) => freeVariables(e)
  case Match(scrutinee, clauses, default) => freeVariables(scrutinee) ++ clauses.flatMap { case (pattern, lit) => freeVariables(lit) } ++ default.toSet.flatMap(s => freeVariables(s))
  case Hole => Set.empty
  case State(id, init, region, body) => freeVariables(init) ++ freeVariables(body) -- Set(id, region)
  case Try(body, tpe, handlers) => freeVariables(body) ++ handlers.flatMap(freeVariables)
  case Region(body) => freeVariables(body)
}

def freeVariables(expr: Expr): Set[Symbol] = expr match {
  case Run(s, tpe) => freeVariables(s)
  case DirectApp(b, targs, args) => freeVariables(b) ++ args.flatMap(freeVariables)
  case pure: Pure => freeVariables(pure)
}

def freeVariables(pure: Pure): Set[Symbol] = pure match {
  case ValueVar(id) => Set(id)
  case literal: Literal[_] => Set.empty
  case PureApp(b, targs, args) => freeVariables(b) ++ args.flatMap(freeVariables)
  case Select(target, field) => freeVariables(target) // we do not count fields in...
  case Box(b) => freeVariables(b) // well, well, well...
}

def freeVariables(arg: Argument): Set[Symbol] = arg match {
  case pure: Pure => freeVariables(pure)
  case block: Block => freeVariables(block)
  case ev: Evidence => freeVariables(ev)
}

def freeVariables(block: Block): Set[Symbol] = block match {
  case BlockVar(id) => Set(id)
  case BlockLit(params, body) =>
    val bound = params.map {
      case ValueParam(id, tpe) => id
      case BlockParam(id, tpe) => id
      case EvidenceParam(id) => id
    }
    freeVariables(body) -- bound
  case Member(b, field) => freeVariables(b)
  case Unbox(e) => freeVariables(e) // TODO well, well, well...
  case New(impl) => freeVariables(impl) // TODO (see also e2c5547b32e40697cafaec51f8e3c27ce639055e)
}

def freeVariables(op: Operation): Set[Symbol] = op match {
  case Operation(name, body) => freeVariables(body)
}

def freeVariables(impl: Implementation): Set[Symbol] = impl match {
  case Implementation(id, operations) => operations.flatMap(freeVariables).toSet
}

def freeVariables(ev: Evidence): Set[Symbol] = ev.scopes.toSet

