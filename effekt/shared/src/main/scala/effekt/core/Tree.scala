package effekt
package core

import effekt.util.Visitor


/**
 * Tree structure of programs in our internal core representation.
 *
 * Core uses [[effekt.symbols.Symbol]] as names. The structure of symbols and the contents
 * in the DB should not be used after translation to core.
 *
 * ----------[[ effekt.core.Tree ]]----------
 *
 *   ─ [[ Tree ]]
 *     │─ [[ ModuleDecl ]]
 *     │─ [[ Declaration ]]
 *     │  │─ [[ Data ]]
 *     │  │─ [[ Interface ]]
 *     │
 *     │─ [[ Constructor ]]
 *     │─ [[ Field ]]
 *     │─ [[ Property ]]
 *     │─ [[ Extern ]]
 *     │  │─ [[ Def ]]
 *     │  │─ [[ Include ]]
 *     │
 *     │─ [[ Argument ]]
 *     │  │─ [[ Pure ]]
 *     │  │─ [[ Block ]]
 *     │
 *     │─ [[ Expr ]]
 *     │  │─ [[ DirectApp ]]
 *     │  │─ [[ Run ]]
 *     │  │─ [[ Pure ]]
 *     │
 *     │─ [[ Param ]]
 *     │  │─ [[ ValueParam ]]
 *     │  │─ [[ BlockParam ]]
 *     │
 *     │─ [[ Stmt ]]
 *     │  │─ [[ Scope ]]
 *     │  │─ [[ Return ]]
 *     │  │─ [[ Val ]]
 *     │  │─ [[ App ]]
 *     │  │─ [[ If ]]
 *     │  │─ [[ Match ]]
 *     │  │─ [[ State ]]
 *     │  │─ [[ Try ]]
 *     │  │─ [[ Region ]]
 *     │  │─ [[ Hole ]]
 *     │
 *     │─ [[ Implementation ]]
 *
 * -------------------------------------------
 */
sealed trait Tree

/**
 * In core, all symbols are supposed to be "just" names.
 */
type Id = symbols.Symbol
object Id {
  def apply(n: String): Id = new symbols.Symbol {
    val name = symbols.Name.local(n)
  }
}

/**
 * A module declaration, the path should be an Effekt include path, not a system dependent file path
 */
case class ModuleDecl(
  path: String,
  imports: List[String],
  declarations: List[Declaration],
  externs: List[Extern],
  definitions: List[Definition],
  exports: List[Id]
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

/**
 * FFI external definitions
 */
enum Extern extends Tree {
  case Def(id: Id, tparams: List[Id], cparams: List[Id], vparams: List[Param.ValueParam], bparams: List[Param.BlockParam], ret: ValueType, annotatedCapture: Captures, body: String)
  case Include(contents: String)
}


enum Definition {
  case Def(id: Id, block: Block)
  case Let(id: Id, binding: Expr) // PURE on the toplevel?

  // TBD
  // case Var(id: Symbol,  region: Symbol, init: Pure) // TOPLEVEL could only be {global}, or not at all.

  // TDB
  // case Mutual(defs: List[Definition.Def])
  val capt: Captures = Type.inferCapt(this)
}

// Some smart constructors
private def addToScope(definition: Definition, body: Stmt): Stmt = body match {
  case Scope(definitions, body) => Scope(definition :: definitions, body)
  case other => Scope(List(definition), other)
}

def Def(id: Id, block: Block, rest: Stmt) =
  addToScope(Definition.Def(id, block), rest)

def Let(id: Id, binding: Expr, rest: Stmt) =
  addToScope(Definition.Let(id,  binding), rest)

/**
 * Fine-grain CBV: Arguments can be either pure expressions [[Pure]] or blocks [[Block]]
 */
sealed trait Argument extends Tree


/**
 * Expressions (with potential IO effects)
 *
 * - [[DirectApp]]
 * - [[Run]]
 * - [[Pure]]
 */
sealed trait Expr extends Tree {
  val tpe: ValueType = Type.inferType(this)
  val capt: Captures = Type.inferCapt(this)
}

// invariant, block b is {io}.
case class DirectApp(b: Block, targs: List[ValueType], vargs: List[Pure], bargs: List[Block]) extends Expr

// only inserted by the transformer if stmt is pure / io
case class Run(s: Stmt) extends Expr


/**
 * Pure Expressions (no IO effects, or control effects)
 *
 * ----------[[ effekt.core.Pure ]]----------
 *
 *   ─ [[ Pure ]]
 *     │─ [[ ValueVar ]]
 *     │─ [[ Literal ]]
 *     │─ [[ PureApp ]]
 *     │─ [[ Select ]]
 *     │─ [[ Box ]]
 *
 * -------------------------------------------
 */
enum Pure extends Expr with Argument {
  case ValueVar(id: Id, annotatedType: ValueType)

  case Literal(value: Any, annotatedType: ValueType)

  // invariant, block b is pure.
  case PureApp(b: Block, targs: List[ValueType], vargs: List[Pure])
  case Select(target: Pure, field: Id, annotatedType: ValueType)

  case Box(b: Block, annotatedCapture: Captures)
}
export Pure.*

/**
 * Blocks
 *
 * ----------[[ effekt.core.Block ]]----------
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
enum Block extends Argument {
  case BlockVar(id: Id, annotatedTpe: BlockType, annotatedCapt: Captures)
  case BlockLit(tparams: List[Id], cparams: List[Id], vparams: List[Param.ValueParam], bparams: List[Param.BlockParam], body: Stmt)
  case Member(block: Block, field: Id, annotatedTpe: BlockType)
  case Unbox(pure: Pure)
  case New(impl: Implementation)


  val tpe: BlockType = Type.inferType(this)
  val capt: Captures = Type.inferCapt(this)
}
export Block.*

enum Param extends Tree {
  def id: Id

  case ValueParam(id: Id, tpe: ValueType)
  case BlockParam(id: Id, tpe: BlockType)
}
export Param.*

/**
 * Statements
 *
 * ----------[[ effekt.core.Stmt ]]----------
 *
 *   ─ [[ Stmt ]]
 *     │─ [[ Scope ]]
 *     │─ [[ Return ]]
 *     │─ [[ Val ]]
 *     │─ [[ App ]]
 *     │─ [[ If ]]
 *     │─ [[ Match ]]
 *     │─ [[ State ]]
 *     │─ [[ Try ]]
 *     │─ [[ Region ]]
 *     │─ [[ Hole ]]
 *
 * -------------------------------------------
 */
enum Stmt extends Tree {

  case Scope(definitions: List[Definition], body: Stmt)

  // Fine-grain CBV
  case Return(expr: Pure)
  case Val(id: Id, binding: Stmt, body: Stmt)
  case App(callee: Block, targs: List[ValueType], vargs: List[Pure], bargs: List[Block])

  // Local Control Flow
  case If(cond: Pure, thn: Stmt, els: Stmt)
  case Match(scrutinee: Pure, clauses: List[(Id, BlockLit)], default: Option[Stmt])

  // Effects
  case State(id: Id, init: Pure, region: Id, body: Stmt) // TODO maybe rename to Var?
  case Try(body: Block, handlers: List[Implementation])
  case Region(body: Block)

  // Others
  case Hole()

  val tpe: ValueType = Type.inferType(this)
  val capt: Captures = Type.inferCapt(this)
}
export Stmt.*

/**
 * An instance of an interface, concretely implementing the operations.
 *
 * Used to represent handlers / capabilities, and objects / modules.
 */
case class Implementation(interface: BlockType.Interface, operations: List[Operation]) extends Tree {
  val tpe = interface
  val capt = operations.flatMap(_.capt).toSet
}

/**
 * Implementation of a method / effect operation.
 *
 * TODO generalize from BlockLit to also allow block definitions
 *
 * TODO For handler implementations we cannot simply reuse BlockLit here...
 *   maybe we need to add PlainOperation | ControlOperation, where for now
 *   handlers always have control operations and New always has plain operations.
 */
case class Operation(name: Id, tparams: List[Id], cparams: List[Id], vparams: List[Param.ValueParam], bparams: List[Param.BlockParam], resume: Option[Param.BlockParam], body: Stmt) {
  val capt = body.capt -- cparams.toSet
}


object Tree {

  // Generic traversal of trees, applying the partial function `f` to every contained
  // element of type Tree.
  def visit(obj: Any)(f: PartialFunction[Tree, Unit]): Unit = obj match {
    case t: Tree if f.isDefinedAt(t) => f(t)
    case s: symbols.Symbol => ()
    case t: Iterable[t] => t.foreach { t => visit(t)(f) }
    case p: Product => p.productIterator.foreach {
      case t => visit(t)(f)
    }
    case leaf => ()
  }

  class Rewrite extends Visitor {
    def pure: PartialFunction[Pure, Pure] = PartialFunction.empty
    def expr: PartialFunction[Expr, Expr] = PartialFunction.empty
    def stmt: PartialFunction[Stmt, Stmt] = PartialFunction.empty
    def defn: PartialFunction[Definition, Definition] = PartialFunction.empty
    def block: PartialFunction[Block, Block] = PartialFunction.empty

    def handler: PartialFunction[Implementation, Implementation] = PartialFunction.empty

    def rewrite(p: Pure): Pure = structural(p, pure)
    def rewrite(e: Expr): Expr = structural(e, expr)
    def rewrite(s: Stmt): Stmt = structural(s, stmt)
    def rewrite(b: Block): Block = structural(b, block)
    def rewrite(d: Definition): Definition = structural(d, defn)

    def rewrite(matchClause: (Id, BlockLit)): (Id, BlockLit) = matchClause match {
      case (p, b) => (p, rewrite(b).asInstanceOf[BlockLit])
    }

    // TODO generate those:
    def rewrite(e: Implementation): Implementation = e match {
      case e if handler.isDefinedAt(e) => handler(e)
      case Implementation(tpe, clauses) => Implementation(tpe, clauses map rewrite)
    }

    def rewrite(o: Operation): Operation = o match {
      case Operation(name, tps, cps, vps, bps, resume, body) => Operation(name, tps, cps, vps, bps, resume, rewrite(body))
    }
  }
}
