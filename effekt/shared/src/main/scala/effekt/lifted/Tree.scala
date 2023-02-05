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
case class Field(id: Id, tpe: ValueType) extends Tree
case class Property(id: Id, tpe: BlockType) extends Tree

enum Param extends Tree {
  def id: Id

  case ValueParam(id: Id, tpe: ValueType)
  case BlockParam(id: Id, tpe: BlockType)
  case EvidenceParam(id: Id)
}
export Param.*

/**
 * FFI external definitions
 */
enum Extern {
  // WARNING: builtins do not take evidence. If they are passed as function argument, they need to be eta-expanded.
  //   (however, if they _would_ take evidence, we could model mutable state with this)
  // TODO revisit
  case Def(id: Symbol, tparams: List[Id], vparams: List[Param.ValueParam], bparams: List[Param.BlockParam], ret: ValueType, body: String)
  case Include(contents: String)
}

enum Definition {
  case Def(id: Symbol, block: Block)
  case Let(id: Symbol, binding: Expr)
}

/**
 * Fine-grain CBV: Arguments can be either expressions or blocks
 */
sealed trait Argument extends Tree

/**
 * Expressions
 */
sealed trait Expr extends Argument {
  def tpe: ValueType = ???
}
case class ValueVar(id: Symbol, annotatedType: ValueType) extends Expr

case class Literal(value: Any, annotatedType: ValueType) extends Expr
case class PureApp(b: Block, targs: List[ValueType], args: List[Argument]) extends Expr
case class Select(target: Expr, field: Symbol, annotatedType: ValueType) extends Expr
case class Box(b: Block) extends Expr
case class Run(s: Stmt) extends Expr

/**
 * Blocks
 */
enum Block extends Argument  {

  case BlockVar(id: Symbol, annotatedType: BlockType)
  case BlockLit(tparams: List[Symbol], params: List[Param], body: Stmt)
  case Member(b: Block, field: Symbol, annotatedTpe: BlockType)

  // WARNING not officially supported, yet
  case Unbox(e: Expr)
  case New(impl: Implementation)

  def tpe: BlockType = ???
}
export Block.*

/**
 * Statements
 */
enum Stmt extends Tree  {
  case Scope(definitions: List[Definition], body: Stmt)

  // Fine-grain CBV
  case Return(e: Expr)
  case Val(id: Symbol, binding: Stmt, body: Stmt)
  case App(b: Block, targs: List[ValueType], args: List[Argument])

  // Local Control Flow
  case If(cond: Expr, thn: Stmt, els: Stmt)
  case Match(scrutinee: Expr, clauses: List[(Symbol, BlockLit)], default: Option[Stmt])

  // Effects
  case State(id: Symbol, init: Expr, region: Symbol, body: Stmt)
  case Try(body: Block, handler: List[Implementation])
  case Region(body: Block)

  // Others
  case Hole()

  def tpe: ValueType = ???
}
export Stmt.*

/**
 * An instance of an interface, concretely implementing the operations.
 *
 * Used to represent handlers / capabilities, and objects / modules.
 */
case class Implementation(id: BlockType.Interface, operations: List[Operation]) extends Tree

/**
 * Implementation of a method / effect operation.
 */
case class Operation(name: symbols.Symbol, implementation: Block.BlockLit)



/**
 * Evidence for lifts
 */
case class Evidence(scopes: List[EvidenceSymbol]) extends Argument

def Here() = Evidence(Nil)

class EvidenceSymbol() extends Symbol { val name = Name.local(s"ev${id}") }

def freeVariables(d: Definition): Set[Param] = d match {
  case Definition.Def(id, block) => freeVariables(block)
  case Definition.Let(id, binding) => freeVariables(binding)
}

def freeVariables(stmt: Stmt): Set[Param] = stmt match {
  // TODO fix
  case Scope(definitions, body) =>
    var free: Set[Param] = Set.empty
    // we assume definitions can be mutually recursive, for now.
    var bound: Set[Param] = definitions.collect { case Definition.Def(id, block) => BlockParam(id, block.tpe) }.toSet
    definitions.foreach {
      case Definition.Def(id, block) =>
        free ++= freeVariables(block) -- bound
      case Definition.Let(id, binding) =>
        free ++= freeVariables(binding) -- bound
        bound ++= Set(ValueParam(id, binding.tpe))
    }
    freeVariables(body) -- bound ++ free
  case Val(id, binding, body) => freeVariables(binding) ++ freeVariables(body) -- Set(ValueParam(id, binding.tpe))
  case App(b, targs, args) => freeVariables(b) ++ args.flatMap(freeVariables)
  case If(cond, thn, els) => freeVariables(cond) ++ freeVariables(thn) ++ freeVariables(els)
  case Return(e) => freeVariables(e)
  case Match(scrutinee, clauses, default) => freeVariables(scrutinee) ++ clauses.flatMap { case (pattern, lit) => freeVariables(lit) } ++ default.toSet.flatMap(s => freeVariables(s))
  case Hole() => Set.empty
  case State(id, init, region, body) =>
    freeVariables(init) ++ freeVariables(body) --
      Set(BlockParam(id, BlockType.Interface(symbols.builtins.TState.interface, List(init.tpe))),
        BlockParam(region, BlockType.Interface(symbols.builtins.RegionSymbol, Nil)))
  case Try(body, handlers) => freeVariables(body) ++ handlers.flatMap(freeVariables)
  case Region(body) => freeVariables(body)
}

def freeVariables(expr: Expr): Set[Param] = expr match {
  case ValueVar(id, tpe) => Set(ValueParam(id, tpe))
  case Literal(value, tpe) => Set.empty
  case PureApp(b, targs, args) => freeVariables(b) ++ args.flatMap(freeVariables)
  case Select(target, field, tpe) => freeVariables(target) // we do not count fields in...
  case Box(b) => freeVariables(b) // well, well, well...
  case Run(s) => freeVariables(s)
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

