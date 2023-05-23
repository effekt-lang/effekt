package effekt
package lifted

import effekt.context.Context
import effekt.symbols.{ Constructor, Name, Symbol }
import scala.collection.immutable

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
  def id: Id
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
  case State(id: Id, init: Expr, region: Symbol, ev: Evidence, body: Stmt)
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

case class FreeVariables(vars: immutable.HashMap[Id, lifted.Param]) {
  def ++(o: FreeVariables): FreeVariables = {
    FreeVariables(vars.merged(o.vars){ case ((leftId -> leftParam), (rightId -> rightParam)) =>
      assert(leftParam == rightParam, "Same id occurs free with different types.")
      (leftId -> leftParam)
    })
  }
  def --(o: FreeVariables): FreeVariables = {
    FreeVariables(vars.filter{ case (leftId -> leftParam) =>
      if(o.vars.contains(leftId)) {
        //assert(leftParam == o.vars(leftId), s"Id bound with different type ${o.vars(leftId)} than it's occurences ${leftParam}.")
        false
      } else { true }
    })
  }

  def -(id: Id) = FreeVariables(vars - id)

  def toList: List[lifted.Param] = vars.values.toList
  // TODO add further accessors as needed
}
object FreeVariables {
  def empty = FreeVariables(immutable.HashMap.empty)
  def apply(ps: Param*): FreeVariables = ps.toFV
  extension (i: Iterable[lifted.Param]) def toFV: FreeVariables = {
    FreeVariables(immutable.HashMap.from(i.map { p => p.id -> p }))
  }
  extension(self: Iterable[FreeVariables]) def combineFV: FreeVariables = {
      self.fold(FreeVariables.empty)(_ ++ _)
  }
}
import FreeVariables.{toFV, combineFV}
def freeVariables(d: Definition): FreeVariables = d match {
  case Definition.Def(id, block) => freeVariables(block) - id // recursive definitions
  case Definition.Let(id, binding) => freeVariables(binding)
}

def freeVariables(stmt: Stmt): FreeVariables = stmt match {
  // TODO fix
  case Scope(definitions, body) =>
    var free: FreeVariables = FreeVariables.empty
    // we assume definitions can be mutually recursive, for now.
    var bound: FreeVariables = definitions.collect { case Definition.Def(id, block) => BlockParam(id, block.tpe) }.toFV
    definitions.foreach {
      case Definition.Def(id, block) =>
        free ++= freeVariables(block) -- bound
      case Definition.Let(id, binding) =>
        free ++= freeVariables(binding) -- bound
        bound ++= FreeVariables(ValueParam(id, binding.tpe))
    }
    freeVariables(body) -- bound ++ free
  case Val(id, binding, body) => freeVariables(binding) ++ freeVariables(body) -- FreeVariables(ValueParam(id, binding.tpe))
  case App(b, targs, args) => freeVariables(b) ++ args.map(freeVariables).combineFV
  case If(cond, thn, els) => freeVariables(cond) ++ freeVariables(thn) ++ freeVariables(els)
  case Return(e) => e.foldLeft(Set.empty[Symbol]) { case (acc, e) => acc ++ freeVariables(e) } // <tODO>: flatmap
  case Match(scrutinee, clauses, default) => freeVariables(scrutinee) ++ clauses.map { case (pattern, lit) => freeVariables(lit) }.combineFV ++ default.toSet.map(s => freeVariables(s)).combineFV
  case Hole() => FreeVariables.empty
  case State(id, init, region, ev, body) =>
    freeVariables(init) ++ freeVariables(ev) ++ freeVariables(body) --
      FreeVariables(BlockParam(id, lifted.BlockType.Interface(symbols.builtins.TState.interface, List(init.tpe))),
        BlockParam(region, lifted.BlockType.Interface(symbols.builtins.RegionSymbol, Nil)))
  case Try(body, handlers) => freeVariables(body) ++ handlers.map(freeVariables).combineFV
  case Shift(ev, body) => freeVariables(ev) ++ freeVariables(body)
  case Region(body) => freeVariables(body)
}

def freeVariables(expr: Expr): FreeVariables = expr match {
  case ValueVar(id, tpe) => FreeVariables(ValueParam(id, tpe))
  case Literal(value, tpe) => FreeVariables.empty
  case PureApp(b, targs, args) => freeVariables(b) ++ args.map(freeVariables).combineFV
  case Select(target, field, tpe) => freeVariables(target) // we do not count fields in...
  case Box(b) => freeVariables(b) // well, well, well...
  case Run(s) => freeVariables(s)
}

def freeVariables(arg: Argument): FreeVariables = arg match {
  case expr: Expr => freeVariables(expr)
  case block: Block => freeVariables(block)
  case ev: Evidence => freeVariables(ev)
}

def freeVariables(block: Block): FreeVariables = block match {
  case BlockVar(id, tpe) => FreeVariables(BlockParam(id, tpe))
  case BlockLit(tparams, params, body) =>
    freeVariables(body) -- params.toFV
  case Member(b, field, tpe) => freeVariables(b)
  case Unbox(e) => freeVariables(e) // TODO well, well, well...
  case New(impl) => freeVariables(impl) // TODO (see also e2c5547b32e40697cafaec51f8e3c27ce639055e)
}

def freeVariables(op: Operation): FreeVariables = op match {
  case Operation(name, body) => freeVariables(body)
}

def freeVariables(impl: Implementation): FreeVariables = impl match {
  case Implementation(id, operations) => operations.map(freeVariables).combineFV
}

def freeVariables(ev: Evidence): FreeVariables = ev.lifts.flatMap {
  case Lift.Var(ev) => List(lifted.Param.EvidenceParam(ev))
  case Lift.Try() => List()
  case Lift.Reg() => List()
}.toFV


def freeTypeVariables(d: Declaration): Set[Id] = d match {
  case Declaration.Data(id, tparams, constructors) => constructors.flatMap(freeTypeVariables).toSet -- Set(id) -- tparams.toSet
  case Declaration.Interface(id, tparams, properties) => properties.flatMap(freeTypeVariables).toSet -- Set(id) -- tparams.toSet
}

def freeTypeVariables(c: Constructor): Set[Id] = c.fields.flatMap(f => freeTypeVariables(f.tpe)).toSet
def freeTypeVariables(p: Property): Set[Id] = freeTypeVariables(p.tpe)


def freeTypeVariables(tpe: ValueType): Set[Id] = tpe match {
  case ValueType.Var(name) => Set(name)
  case ValueType.Data(name, targs) => Set(name) ++ targs.toSet.flatMap(freeTypeVariables)
  case ValueType.Boxed(tpe) => freeTypeVariables(tpe)
}
def freeTypeVariables(tpe: BlockType): Set[Id] = tpe match {
  case BlockType.Function(tparams, eparams, vparams, bparams, result) =>
    (vparams.flatMap(freeTypeVariables).toSet ++ bparams.flatMap(freeTypeVariables).toSet ++ freeTypeVariables(result)) -- tparams.toSet
  case BlockType.Interface(name, targs) => Set(name) ++ targs.toSet.flatMap(freeTypeVariables)
}
