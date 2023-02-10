package effekt
package lifted

import effekt.context.Context
import effekt.symbols.{ Constructor, Name, Symbol }

export effekt.core.Id

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
  case Def(id: Id, tparams: List[Id], params: List[Param], ret: ValueType, body: String)
  case Include(contents: String)
}

enum Definition {
  case Def(id: Id, block: Block)
  case Let(id: Id, binding: Expr)
}

/**
 * Fine-grain CBV: Arguments can be evidence, expressions, or blocks
 */
sealed trait Argument extends Tree

/**
 * Expressions
 */
enum Expr extends Argument {
  case ValueVar(id: Id, annotatedType: ValueType)
  case Literal(value: Any, annotatedType: ValueType)

  // invariant, block b is known to be pure; does not take evidence, does not use IO resources.
  case PureApp(b: Block, targs: List[ValueType], args: List[Argument])
  case Select(target: Expr, field: Id, annotatedType: ValueType)
  case Box(b: Block)

  case Run(s: Stmt)

  val tpe: ValueType = Type.inferType(this)
}
export Expr.*


// TODO
//   case class DirectApp(b: Block, targs: List[ValueType], args: List[Argument]) extends Expr


/**
 * Blocks
 */
enum Block extends Argument  {

  case BlockVar(id: Id, annotatedType: BlockType)
  case BlockLit(tparams: List[Id], params: List[Param], body: Stmt)
  case Member(b: Block, field: Id, annotatedTpe: BlockType)

  // WARNING not officially supported, yet
  case Unbox(e: Expr)
  case New(impl: Implementation)

  val tpe: BlockType = Type.inferType(this)
}
export Block.*

/**
 * Statements
 */
enum Stmt extends Tree  {
  case Scope(definitions: List[Definition], body: Stmt)

  // Fine-grain CBV
  case Return(e: Expr)
  case Val(id: Id, binding: Stmt, body: Stmt)
  case App(b: Block, targs: List[ValueType], args: List[Argument])

  // Local Control Flow
  case If(cond: Expr, thn: Stmt, els: Stmt)
  case Match(scrutinee: Expr, clauses: List[(Symbol, BlockLit)], default: Option[Stmt])

  // Effects
  case State(id: Id, init: Expr, region: Symbol, body: Stmt)
  case Try(body: Block, handler: List[Implementation])
  case Region(body: Block)

  // e.g. shift(ev) { {resume} => ... }
  case Shift(ev: Evidence, body: Block.BlockLit)

  // Others
  case Hole()

  val tpe: ValueType = Type.inferType(this)
}
export Stmt.*

/**
 * An instance of an interface, concretely implementing the operations.
 *
 * Used to represent handlers / capabilities, and objects / modules.
 */
case class Implementation(interface: BlockType.Interface, operations: List[Operation]) extends Tree {
  val tpe = interface
}

/**
 * Implementation of a method / effect operation.
 */
case class Operation(name: symbols.Symbol, implementation: Block.BlockLit)



enum Lift {
  case Var(ev: EvidenceSymbol)
  case Try()
  case Reg()
}

/**
 * Evidence for lifts
 */
case class Evidence(lifts: List[Lift]) extends Argument

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
  case Shift(ev, body) => freeVariables(ev) ++ freeVariables(body)
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

def freeVariables(ev: Evidence): Set[Param] = ev.lifts.toSet.collect {
  case Lift.Var(ev) => EvidenceParam(ev)
}

