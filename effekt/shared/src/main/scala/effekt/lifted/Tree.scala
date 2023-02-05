package effekt
package lifted

import effekt.context.Context
import effekt.symbols.{ Constructor, Name, Symbol }

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
case class Field(id: Id, tpe: typed.ValueType) extends Tree
case class Property(id: Id, tpe: typed.BlockType) extends Tree

/**
 * FFI external definitions
 */
enum Extern {
  case Def(id: Symbol, tpe: core.BlockType.Function, params: List[Param], body: String)
  case Include(contents: String)
}

enum Definition {
  case Def(id: Symbol, tpe: core.BlockType, block: Block)
  case Let(id: Symbol, tpe: core.ValueType, binding: Expr)
}

/**
 * Fine-grain CBV: Arguments can be either expressions or blocks
 */
sealed trait Argument extends Tree

/**
 * Expressions
 */
sealed trait Expr extends Argument
case class ValueVar(id: Symbol, tpe: core.ValueType) extends Expr

case class Literal(value: Any, tpe: core.ValueType) extends Expr
case class PureApp(b: Block, targs: List[core.ValueType], args: List[Argument]) extends Expr
case class Select(target: Expr, field: Symbol, tpe: core.ValueType) extends Expr
case class Box(b: Block) extends Expr
case class Run(s: Stmt, tpe: core.ValueType) extends Expr

/**
 * Blocks
 */
sealed trait Param extends Tree { def id: Symbol }
case class ValueParam(id: Symbol, tpe: core.ValueType) extends Param
case class BlockParam(id: Symbol, tpe: core.BlockType) extends Param
case class EvidenceParam(id: EvidenceSymbol) extends Param

sealed trait Block extends Argument
case class BlockVar(id: Symbol, tpe: core.BlockType) extends Block
case class BlockLit(tparams: List[Symbol], params: List[Param], body: Stmt) extends Block
case class Member(b: Block, field: Symbol, annotatedTpe: core.BlockType) extends Block
case class Unbox(e: Expr) extends Block
case class New(impl: Implementation) extends Block

/**
 * Statements
 */
sealed trait Stmt extends Tree

case class Scope(definitions: List[Definition], body: Stmt) extends Stmt

// Fine-grain CBV
case class Return(e: Expr) extends Stmt
case class Val(id: Symbol, tpe: core.ValueType, binding: Stmt, body: Stmt) extends Stmt
case class App(b: Block, targs: List[core.ValueType], args: List[Argument]) extends Stmt

// Local Control Flow
case class If(cond: Expr, thn: Stmt, els: Stmt) extends Stmt
case class Match(scrutinee: Expr, clauses: List[(Symbol, BlockLit)], default: Option[Stmt]) extends Stmt

// Effects
case class State(id: Symbol, init: Expr, stateTpe: core.ValueType, region: Symbol, body: Stmt) extends Stmt
case class Try(body: Block, answerType: core.ValueType, handler: List[Implementation]) extends Stmt
case class Region(body: Block, answerType: core.ValueType) extends Stmt

// Others
case object Hole extends Stmt

/**
 * An instance of an interface, concretely implementing the operations.
 *
 * Used to represent handlers / capabilities, and objects / modules.
 */
case class Implementation(id: core.BlockType.Interface, operations: List[Operation]) extends Tree

/**
 * Implementation of a method / effect operation.
 */
case class Operation(name: symbols.Symbol, implementation: BlockLit)



/**
 * Evidence for lifts
 */
case class Evidence(scopes: List[EvidenceSymbol]) extends Argument

def Here() = Evidence(Nil)

class EvidenceSymbol() extends Symbol { val name = Name.local(s"ev${id}") }

def freeVariables(d: Definition): Set[Param] = d match {
  case Definition.Def(id, tpe, block) => freeVariables(block)
  case Definition.Let(id, tpe, binding) => freeVariables(binding)
}

def freeVariables(stmt: Stmt): Set[Param] = stmt match {
  // TODO fix
  case Scope(definitions, body) =>
    var free: Set[Param] = Set.empty
    // we assume definitions can be mutually recursive, for now.
    var bound: Set[Param] = definitions.collect { case Definition.Def(id, tpe, _) => BlockParam(id, tpe) }.toSet
    definitions.foreach {
      case Definition.Def(id, tpe, block) =>
        free ++= freeVariables(block) -- bound
      case Definition.Let(id, tpe, binding) =>
        free ++= freeVariables(binding) -- bound
        bound ++= Set(ValueParam(id, tpe))
    }
    freeVariables(body) -- bound ++ free
  case Val(id, tpe, binding, body) => freeVariables(binding) ++ freeVariables(body) -- Set(ValueParam(id, tpe))
  case App(b, targs, args) => freeVariables(b) ++ args.flatMap(freeVariables)
  case If(cond, thn, els) => freeVariables(cond) ++ freeVariables(thn) ++ freeVariables(els)
  case Return(e) => freeVariables(e)
  case Match(scrutinee, clauses, default) => freeVariables(scrutinee) ++ clauses.flatMap { case (pattern, lit) => freeVariables(lit) } ++ default.toSet.flatMap(s => freeVariables(s))
  case Hole => Set.empty
  case State(id, init, stateTpe, region, body) =>
    freeVariables(init) ++ freeVariables(body) --
      Set(BlockParam(id, core.BlockType.Interface(symbols.builtins.TState.interface, List(stateTpe))),
        BlockParam(region, core.BlockType.Interface(symbols.builtins.RegionSymbol, Nil)))
  case Try(body, tpe, handlers) => freeVariables(body) ++ handlers.flatMap(freeVariables)
  case Region(body, _) => freeVariables(body)
}

def freeVariables(expr: Expr): Set[Param] = expr match {
  case ValueVar(id, tpe) => Set(ValueParam(id, tpe))
  case Literal(value, tpe) => Set.empty
  case PureApp(b, targs, args) => freeVariables(b) ++ args.flatMap(freeVariables)
  case Select(target, field, tpe) => freeVariables(target) // we do not count fields in...
  case Box(b) => freeVariables(b) // well, well, well...
  case Run(s, tpe) => freeVariables(s)
}

def freeVariables(arg: Argument): Set[Param] = arg match {
  case expr: Expr => freeVariables(expr)
  case block: Block => freeVariables(block)
  case ev: Evidence => freeVariables(ev)
}

def freeVariables(block: Block): Set[Param] = block match {
  case BlockVar(id, tpe) => Set(BlockParam(id, tpe))
  case BlockLit(tparams, params, body) =>
    freeVariables(body) -- params
  case Member(b, field, tpe) => freeVariables(b)
  case Unbox(e) => freeVariables(e) // TODO well, well, well...
  case New(impl) => freeVariables(impl) // TODO (see also e2c5547b32e40697cafaec51f8e3c27ce639055e)
}

def freeVariables(op: Operation): Set[Param] = op match {
  case Operation(name, body) => freeVariables(body)
}

def freeVariables(impl: Implementation): Set[Param] = impl match {
  case Implementation(id, operations) => operations.flatMap(freeVariables).toSet
}

def freeVariables(ev: Evidence): Set[Param] = ev.scopes.toSet.map(EvidenceParam(_))

