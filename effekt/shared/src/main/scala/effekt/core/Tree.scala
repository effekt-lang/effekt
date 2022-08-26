package effekt
package core

import effekt.context.Context
import effekt.symbols.{ Name, Symbol, TermSymbol, ValueSymbol, BlockSymbol, Interface, InterfaceType, Operation, Type, ValueType, FunctionType, BlockType, TrackedParam }

sealed trait Tree extends Product {
  def inheritPosition(from: source.Tree)(implicit C: Context): this.type = {
    C.positions.dupPos(from, this);
    this
  }
}

/**
 * A module declaration, the path should be an Effekt include path, not a system dependent file path
 */
case class ModuleDecl(path: String, imports: List[String], defs: Stmt, exports: List[Symbol]) extends Tree

/**
 * Fine-grain CBV: Arguments can be either pure expressions or blocks
 */
sealed trait Argument extends Tree

/**
 * Expressions (with potential IO effects)
 */
sealed trait Expr extends Tree

// invariant, block b is {io}.
case class DirectApp(b: Block, targs: List[Type], args: List[Argument]) extends Expr

// only inserted by the transformer if stmt is pure / io
case class Run(s: Stmt) extends Expr

/**
 * Pure Expressions (no IO effects, or control effects)
 */
sealed trait Pure extends Expr with Argument
case class ValueVar(id: ValueSymbol) extends Pure

sealed trait Literal[T] extends Pure {
  def value: T
}
case class UnitLit() extends Literal[Unit] { def value = () }
case class IntLit(value: Int) extends Literal[Int]
case class BooleanLit(value: Boolean) extends Literal[Boolean]
case class DoubleLit(value: Double) extends Literal[Double]
case class StringLit(value: String) extends Literal[String]

// invariant, block b is pure.
case class PureApp(b: Block, targs: List[Type], args: List[Pure]) extends Pure
case class Select(target: Pure, field: Symbol) extends Pure

case class Box(b: Block) extends Pure

/**
 * Blocks
 */
sealed trait Param extends Tree { def id: TermSymbol }
case class ValueParam(id: ValueSymbol, tpe: ValueType) extends Param
case class BlockParam(id: BlockSymbol, tpe: BlockType) extends Param

sealed trait Block extends Tree with Argument
case class BlockVar(id: BlockSymbol) extends Block
case class BlockLit(params: List[Param], body: Stmt) extends Block
case class Member(b: Block, field: TermSymbol) extends Block
case class Extern(params: List[Param], body: String) extends Block
case class Unbox(p: Pure) extends Block
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

case class If(cond: Pure, thn: Stmt, els: Stmt) extends Stmt
case class While(cond: Stmt, body: Stmt) extends Stmt
case class Ret(e: Pure) extends Stmt
case class Match(scrutinee: Pure, clauses: List[(Pattern, BlockLit)]) extends Stmt

sealed trait Pattern extends Tree
case class IgnorePattern() extends Pattern
case class AnyPattern() extends Pattern
case class TagPattern(tag: Symbol, patterns: List[Pattern]) extends Pattern
case class LiteralPattern[T](l: Literal[T]) extends Pattern

case class Include(contents: String, rest: Stmt) extends Stmt

case object Hole extends Stmt

case class State(id: Symbol, init: Pure, region: Symbol, body: Stmt) extends Stmt
case class Handle(body: Block, handler: List[Handler]) extends Stmt
// TODO change to Map
case class Handler(id: Interface, clauses: List[(Operation, BlockLit)]) extends Tree

case class Region(body: Block) extends Stmt

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
    def pattern: PartialFunction[Pattern, Pattern] = PartialFunction.empty
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
        case Run(s)  => Run(rewrite(s))
        case p: Pure => rewrite(p)
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
        case Data(id, ctors, rest) =>
          Data(id, ctors, rewrite(rest))
        case Record(id, fields, rest) =>
          Record(id, fields, rewrite(rest))
        case App(b, targs, args) =>
          App(rewrite(b), targs, args map rewrite)
        case If(cond, thn, els) =>
          If(rewrite(cond), rewrite(thn), rewrite(els))
        case While(cond, body) =>
          While(rewrite(cond), rewrite(body))
        case Ret(e: Expr) =>
          Ret(rewrite(e))
        case Include(contents, rest) =>
          Include(contents, rewrite(rest))
        case State(id, init, reg, body) =>
          State(id, rewrite(init), reg, rewrite(body))
        case Handle(body, handler) =>
          Handle(rewrite(body), handler map rewrite)
        case Region(body) =>
          Region(rewrite(body))
        case Match(scrutinee, clauses) =>
          Match(rewrite(scrutinee), clauses map {
            case (p, b) => (p, rewrite(b).asInstanceOf[BlockLit])
          })
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
      case Extern(params, body) =>
        Extern(params map rewrite, body)
      case Unbox(e) =>
        Unbox(rewrite(e))
      case New(impl) =>
        New(rewrite(impl))
      case b: BlockVar => b
    }
    def rewrite(e: Pattern): Pattern = e match {
      case e if pattern.isDefinedAt(e) => pattern(e)
      case TagPattern(tag, patterns: List[Pattern]) =>
        TagPattern(tag, patterns map rewrite)
      case LiteralPattern(l) =>
        LiteralPattern(rewrite(l).asInstanceOf[Literal[_]])
      case p => p
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
