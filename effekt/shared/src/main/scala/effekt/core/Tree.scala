package effekt
package core

import effekt.symbols.{ BlockSymbol, FunctionType, BlockType, Constructor, Interface, Operation, Symbol, TermSymbol, Type, ValueSymbol, ValueType }

/**
 * Tree structure of programs in our internal core representation.
 *
 * ----------[[ effekt.core.Tree ]]----------
 *
 *   ─ [[ Tree ]]
 *     │─ [[ ModuleDecl ]]
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
 *     │  │─ [[ Return ]]
 *     │  │─ [[ Val ]]
 *     │  │─ [[ Def ]]
 *     │  │─ [[ Let ]]
 *     │  │─ [[ App ]]
 *     │  │─ [[ If ]]
 *     │  │─ [[ While ]]
 *     │  │─ [[ Match ]]
 *     │  │─ [[ State ]]
 *     │  │─ [[ Handle ]]
 *     │  │─ [[ Region ]]
 *     │  │─ [[ Hole ]]
 *     │
 *     │─ [[ Handler ]]
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
 *
 * - [[DirectApp]]
 * - [[Run]]
 * - [[Pure]]
 */
sealed trait Expr extends Tree

// invariant, block b is {io}.
case class DirectApp(b: Block, targs: List[Type], args: List[Argument]) extends Expr

// only inserted by the transformer if stmt is pure / io
case class Run(s: Stmt, tpe: ValueType) extends Expr


/**
 * Pure Expressions (no IO effects, or control effects)
 *
 * ----------[[ effekt.core.Pure ]]----------
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
  case class Select(target: Pure, field: Symbol) extends Pure

  case class Box(b: Block) extends Pure
}
export Pure.*
export Literal.*

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
  case BlockVar(id: BlockSymbol)
  case BlockLit(params: List[Param], body: Stmt)
  case Member(b: Block, field: TermSymbol)
  case Unbox(p: Pure)
  case New(impl: Handler)
}
export Block.*

enum Param extends Tree {
  def id: TermSymbol

  case ValueParam(id: ValueSymbol, tpe: ValueType)
  case BlockParam(id: BlockSymbol, tpe: BlockType)
}
export Param.*

/**
 * Statements
 *
 * ----------[[ effekt.core.Stmt ]]----------
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
 *     │─ [[ Handle ]]
 *     │─ [[ Region ]]
 *     │─ [[ Hole ]]
 *
 * -------------------------------------------
 */
enum Stmt extends Tree {
  // Fine-grain CBV
  case Return(e: Pure)
  case Val(id: ValueSymbol, tpe: ValueType, binding: Stmt, body: Stmt)
  case Def(id: BlockSymbol, tpe: BlockType, block: Block, rest: Stmt)
  case Let(id: ValueSymbol, tpe: ValueType, binding: Expr, body: Stmt)
  case App(b: Block, targs: List[Type], args: List[Argument])

  // Local Control Flow
  case If(cond: Pure, thn: Stmt, els: Stmt)
  case While(cond: Stmt, body: Stmt)
  case Match(scrutinee: Pure, clauses: List[(Constructor, BlockLit)], default: Option[Stmt])

  // Effects
  case State(id: Symbol, init: Pure, region: Symbol, body: Stmt)
  case Handle(body: Block, answerType: ValueType, handler: List[Handler])
  case Region(body: Block)

  // Others
  case Hole
}
export Stmt.*

case class Handler(id: symbols.Interface, clauses: List[(Operation, Block.BlockLit)]) extends Tree


object Tree {

  // Generic traversal of trees, applying the partial function `f` to every contained
  // element of type Tree.
  def visit(obj: Any)(f: PartialFunction[Tree, Unit]): Unit = obj match {
    case t: Iterable[t] => t.foreach { t => visit(t)(f) }
    case p: Product => p.productIterator.foreach {
      case t: Tree => f(t)
      case other   => ()
    }
    case leaf => Set.empty
  }

  // This solution is between a fine-grained visitor and a untyped and unsafe traversal.
  trait Rewrite {
    // Hooks to override
    def pure: PartialFunction[Pure, Pure] = PartialFunction.empty
    def expr: PartialFunction[Expr, Expr] = PartialFunction.empty
    def stmt: PartialFunction[Stmt, Stmt] = PartialFunction.empty
    def param: PartialFunction[Param, Param] = PartialFunction.empty
    def block: PartialFunction[Block, Block] = PartialFunction.empty
    def handler: PartialFunction[Handler, Handler] = PartialFunction.empty

    def rewrite(p: Pure): Pure =
      p match {
        case e if pure.isDefinedAt(e) => pure(e)
        case PureApp(b, targs, args) =>
          PureApp(rewrite(b), targs, args map rewrite)
        case Select(target, field) =>
          Select(rewrite(target), field)
        case v: ValueVar   => v
        case l: Literal[_] => l
        case Box(b)        => Box(rewrite(b))
      }

    // Entrypoints to use the traversal on, defined in terms of the above hooks
    def rewrite(e: Expr): Expr =
      e match {
        case e if expr.isDefinedAt(e) => expr(e)
        case DirectApp(b, targs, args) =>
          DirectApp(rewrite(b), targs, args map rewrite)
        case Run(s, tpe) => Run(rewrite(s), tpe)
        case p: Pure     => rewrite(p)
      }

    def rewrite(e: Stmt): Stmt =
      e match {
        case e if stmt.isDefinedAt(e) => stmt(e)
        case Def(id, tpe, block, rest) =>
          Def(id, tpe, rewrite(block), rewrite(rest))
        case Val(id, tpe, binding, body) =>
          Val(id, tpe, rewrite(binding), rewrite(body))
        case Let(id, tpe, binding, body) =>
          Let(id, tpe, rewrite(binding), rewrite(body))
        case App(b, targs, args) =>
          App(rewrite(b), targs, args map rewrite)
        case If(cond, thn, els) =>
          If(rewrite(cond), rewrite(thn), rewrite(els))
        case While(cond, body) =>
          While(rewrite(cond), rewrite(body))
        case Return(e: Expr) =>
          Return(rewrite(e))
        case State(id, init, reg, body) =>
          State(id, rewrite(init), reg, rewrite(body))
        case Handle(body, tpe, handler) =>
          Handle(rewrite(body), tpe, handler map rewrite)
        case Region(body) =>
          Region(rewrite(body))
        case Match(scrutinee, clauses, default) =>
          Match(rewrite(scrutinee), clauses map {
            case (p, b) => (p, rewrite(b).asInstanceOf[BlockLit])
          }, default map rewrite)
        case h: Hole.type => h
      }

    def rewrite(e: Param): Param = e match {
      case e if param.isDefinedAt(e) => param(e)
      case e => e
    }
    def rewrite(e: Block): Block = e match {
      case e if block.isDefinedAt(e) => block(e)
      case BlockLit(params, body) =>
        BlockLit(params map rewrite, rewrite(body))
      case Member(b, field) =>
        Member(rewrite(b), field)
      case Unbox(e) =>
        Unbox(rewrite(e))
      case New(impl) =>
        New(rewrite(impl))
      case b: BlockVar => b
    }
    def rewrite(e: Handler): Handler = e match {
      case e if handler.isDefinedAt(e) => handler(e)
      case Handler(id: Symbol, clauses: List[(Symbol, BlockLit)]) => Handler(id, clauses map {
        case (s, b) => (s, rewrite(b).asInstanceOf[BlockLit])
      })
    }

    def rewrite(e: Argument): Argument = e match {
      case p: Pure  => rewrite(p)
      case e: Block => rewrite(e)
    }
  }

}
