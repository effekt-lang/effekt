package effekt
package core

import effekt.symbols.{ BlockSymbol, FunctionType, Constructor, Interface, Operation, Symbol, TermSymbol, Type, ValueSymbol }

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
 *     │  │─ [[ Record ]]
 *     │  │─ [[ Interface ]]
 *     │
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
 * A module declaration, the path should be an Effekt include path, not a system dependent file path
 */
case class ModuleDecl(
  path: String,
  imports: List[String],
  declarations: List[Declaration],
  externs: List[Extern],
  definitions: List[Definition],
  exports: List[Symbol]
) extends Tree

/**
 * Toplevel data and interface declarations
 */
enum Declaration extends Tree {
  def id: Symbol

  // TODO also translate constructors, fields and operations to core.
  case Data(id: Symbol, ctors: List[Symbol])
  case Record(id: Symbol, fields: List[Symbol])
  case Interface(id: Symbol, operations: List[Symbol])
}
export Declaration.*

/**
 * FFI external definitions
 */
enum Extern extends Tree {
  case Def(id: Symbol, tparams: List[Symbol], vparams: List[Param.ValueParam], bparams: List[Param.BlockParam], ret: ValueType, annotatedCapture: Captures, body: String)
  case Include(contents: String)
}


enum Definition {
  case Def(id: Symbol, block: Block)
  case Let(id: Symbol, binding: Expr) // PURE on the toplevel?

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

def Def(id: Symbol, block: Block, rest: Stmt) =
  addToScope(Definition.Def(id, block), rest)

def Let(id: Symbol, binding: Expr, rest: Stmt) =
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
  val tpe: core.ValueType = Type.inferType(this)
  val capt: core.Captures = Type.inferCapt(this)
}

// invariant, block b is {io}.
case class DirectApp(b: Block, targs: List[core.ValueType], args: List[Argument]) extends Expr

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
  case ValueVar(id: Symbol, annotatedType: ValueType) extends Pure

  case Literal(value: Any, annotatedType: ValueType) extends Pure

  // invariant, block b is pure.
  case PureApp(b: Block, targs: List[core.ValueType], args: List[Pure]) extends Pure
  case Select(target: Pure, field: symbols.Field) extends Pure

  case Box(b: Block, annotatedCapture: Captures) extends Pure
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
  case BlockVar(id: Symbol, annotatedTpe: BlockType, annotatedCapt: Captures)
  case BlockLit(tparams: List[Symbol], vparams: List[Param.ValueParam], bparams: List[Param.BlockParam], body: Stmt)
  case Member(block: Block, field: symbols.Symbol, annotatedTpe: BlockType)
  case Unbox(pure: Pure)
  case New(impl: Implementation)


  val tpe: core.BlockType = Type.inferType(this)
  val capt: Captures = Type.inferCapt(this)
}
export Block.*

enum Param extends Tree {
  def id: Symbol

  case ValueParam(id: Symbol, tpe: ValueType)
  case BlockParam(id: Symbol, tpe: BlockType)
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
  case Val(id: Symbol, binding: Stmt, body: Stmt)
  case App(callee: Block, targs: List[core.ValueType], args: List[Argument])

  // Local Control Flow
  case If(cond: Pure, thn: Stmt, els: Stmt)
  case Match(scrutinee: Pure, clauses: List[(Constructor, BlockLit)], default: Option[Stmt])

  // Effects
  case State(id: Symbol, init: Pure, region: Symbol, body: Stmt) // TODO maybe rename to Var?
  case Try(body: Block, handlers: List[Implementation])
  case Region(body: Block)

  // Others
  case Hole()

  val tpe: core.ValueType = Type.inferType(this)
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
 */
case class Operation(name: symbols.Operation, implementation: Block.BlockLit) {
  val tpe = implementation.tpe
  val capt = implementation.capt
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

  // This solution is between a fine-grained visitor and a untyped and unsafe traversal.
  trait Rewrite {
    // Hooks to override
    def pure: PartialFunction[Pure, Pure] = PartialFunction.empty
    def expr: PartialFunction[Expr, Expr] = PartialFunction.empty
    def stmt: PartialFunction[Stmt, Stmt] = PartialFunction.empty
    def defn: PartialFunction[Definition, Definition] = PartialFunction.empty
    def block: PartialFunction[Block, Block] = PartialFunction.empty
    def handler: PartialFunction[Implementation, Implementation] = PartialFunction.empty

    def rewrite(p: Pure): Pure =
      p match {
        case e if pure.isDefinedAt(e) => pure(e)
        case PureApp(b, targs, args) =>
          PureApp(rewrite(b), targs, args map rewrite)
        case Select(target, field) =>
          Select(rewrite(target), field)
        case v: ValueVar   => v
        case l: Literal    => l
        case Box(b, capt)        => Box(rewrite(b), capt)
      }

    // Entrypoints to use the traversal on, defined in terms of the above hooks
    def rewrite(e: Expr): Expr =
      e match {
        case e if expr.isDefinedAt(e) => expr(e)
        case DirectApp(b, targs, args) =>
          DirectApp(rewrite(b), targs, args map rewrite)
        case Run(s) => Run(rewrite(s))
        case p: Pure     => rewrite(p)
      }

    def rewrite(d: Definition): Definition = d match {
      case d if defn.isDefinedAt(d) => defn(d)
      case Definition.Def(id, block) =>
        Definition.Def(id, rewrite(block))
      case Definition.Let(id, binding) =>
        Definition.Let(id, rewrite(binding))
    }

    def rewrite(e: Stmt): Stmt =
      e match {
        case e if stmt.isDefinedAt(e) => stmt(e)
        case Scope(definitions, rest) => Scope(definitions map rewrite, rewrite(rest))
        case Val(id, binding, body) =>
          Val(id, rewrite(binding), rewrite(body))
        case App(b, targs, args) =>
          App(rewrite(b), targs, args map rewrite)
        case If(cond, thn, els) =>
          If(rewrite(cond), rewrite(thn), rewrite(els))
        case Return(e: Expr) =>
          Return(rewrite(e))
        case State(id, init, reg, body) =>
          State(id, rewrite(init), reg, rewrite(body))
        case Try(body, handler) =>
          Try(rewrite(body), handler map rewrite)
        case Region(body) =>
          Region(rewrite(body))
        case Match(scrutinee, clauses, default) =>
          Match(rewrite(scrutinee), clauses map {
            case (p, b) => (p, rewrite(b).asInstanceOf[BlockLit])
          }, default map rewrite)
        case Hole() => e
      }

    def rewrite(e: Block): Block = e match {
      case e if block.isDefinedAt(e) => block(e)
      case BlockLit(tps, vps, bps, body) =>
        BlockLit(tps, vps, bps, rewrite(body))
      case Member(b, field, tpe) =>
        Member(rewrite(b), field, tpe)
      case Unbox(e) =>
        Unbox(rewrite(e))
      case New(impl) =>
        New(rewrite(impl))
      case b: BlockVar => b
    }
    def rewrite(e: Implementation): Implementation = e match {
      case e if handler.isDefinedAt(e) => handler(e)
      case Implementation(tpe, clauses) => Implementation(tpe, clauses map rewrite)
    }
    def rewrite(o: Operation): Operation = o match {
      case Operation(name, impl) => Operation(name, rewrite(impl).asInstanceOf[BlockLit])
    }

    def rewrite(e: Argument): Argument = e match {
      case p: Pure  => rewrite(p)
      case e: Block => rewrite(e)
    }
  }

}
