package effekt
package core

import effekt.context.Context
import effekt.symbols.{ Name, Symbol, TermSymbol, ValueSymbol, BlockSymbol, UserEffect, Effect, Method, Type, ValueType, BlockType, InterfaceType }

sealed trait Tree extends Product {
  def inheritPosition(from: source.Tree)(implicit C: Context): this.type = {
    C.positions.dupPos(from, this);
    this
  }
}

/**
 * A module declaration, the path should be an Effekt include path, not a system dependent file path
 */
case class ModuleDecl(path: Name, imports: List[Name], defs: Stmt) extends Tree

/// User-definied module
// TODO: implementierungen merken
// TODO?: body => List[Stmt]
case class UserModule(body: Stmt) extends Block

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

/**
 * Blocks
 */
sealed trait Param extends Tree { def id: TermSymbol }
case class ValueParam(id: ValueSymbol, tpe: ValueType) extends Param
case class BlockParam(id: BlockSymbol, tpe: InterfaceType) extends Param

sealed trait Block extends Tree with Argument
case class BlockVar(id: BlockSymbol) extends Block

// introduced by lift inference only
case class ScopeAbs(scope: Symbol, body: Block) extends Block
case class ScopeApp(b: Block, evidence: Scope) extends Block
case class Lifted(s: Scope, b: Block) extends Block

// TODO add type params here
case class BlockLit(params: List[Param], body: Stmt) extends Block
case class Member(b: Block, field: BlockSymbol) extends Block
case class Extern(params: List[Param], body: String) extends Block
case class Unbox(e: Expr) extends Block

/**
 * Statements
 */
sealed trait Stmt extends Tree
case class Def(id: BlockSymbol, tpe: InterfaceType, block: Block, rest: Stmt) extends Stmt
case class Val(id: ValueSymbol, tpe: ValueType, binding: Stmt, body: Stmt) extends Stmt
case class Data(id: Symbol, ctors: List[Symbol], rest: Stmt) extends Stmt
case class Record(id: Symbol, fields: List[Symbol], rest: Stmt) extends Stmt

case class App(b: Block, targs: List[Type], args: List[Argument]) extends Stmt

case class If(cond: Expr, thn: Stmt, els: Stmt) extends Stmt
case class While(cond: Stmt, body: Stmt) extends Stmt
case class Ret(e: Expr) extends Stmt
case class Exports(path: Name, exports: List[Symbol]) extends Stmt
case class Match(scrutinee: Expr, clauses: List[(Pattern, BlockLit)]) extends Stmt

sealed trait Pattern extends Tree
case class IgnorePattern() extends Pattern
case class AnyPattern() extends Pattern
case class TagPattern(tag: Symbol, patterns: List[Pattern]) extends Pattern
case class LiteralPattern[T](l: Literal[T]) extends Pattern

case class Include(contents: String, rest: Stmt) extends Stmt

case object Hole extends Stmt

case class State(id: UserEffect, tpe: ValueType, get: Method, put: Method, init: Stmt, body: Block) extends Stmt
case class Handle(body: Block, handler: List[Handler]) extends Stmt
// TODO change to Map
case class Handler(id: UserEffect, clauses: List[(Method, BlockLit)]) extends Tree

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

case class ScopeId() extends Symbol { val name = effekt.symbols.builtins.prelude.name.nest(Name(s"ev${id}")) }

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
    def expr: PartialFunction[Expr, Expr] = t => t
    def stmt: PartialFunction[Stmt, Stmt] = t => t
    def param: PartialFunction[Param, Param] = t => t
    def block: PartialFunction[Block, Block] = t => t
    def pattern: PartialFunction[Pattern, Pattern] = t => t
    def handler: PartialFunction[Handler, Handler] = t => t

    // Entrypoints to use the traversal on, defined in terms of the above hooks
    def rewrite(e: Expr): Expr = e match {
      case e if expr.isDefinedAt(e) => expr(e)
      case PureApp(b, targs, args) =>
        PureApp(rewrite(b), targs, args map rewrite)
      case Select(target, field) =>
        Select(rewrite(target), field)
      case v: ValueVar   => v
      case l: Literal[_] => l
    }
    def rewrite(e: Stmt): Stmt = e match {
      case e if stmt.isDefinedAt(e) => stmt(e)
      case Def(id, tpe, block, rest) =>
        Def(id, tpe, rewrite(block), rewrite(rest))
      case Val(id, tpe, binding, body) =>
        Val(id, tpe, rewrite(binding), rewrite(body))
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
      case State(id, tpe, get, put, init, body) =>
        State(id, tpe, get, put, rewrite(init), rewrite(body))
      case Handle(body, handler) =>
        Handle(rewrite(body), handler map rewrite)
      case Match(scrutinee, clauses) =>
        Match(rewrite(scrutinee), clauses map {
          case (p, b) => (p, rewrite(b).asInstanceOf[BlockLit])
        })
      case e: Exports   => e
      case h: Hole.type => h
    }
    def rewrite(e: Param): Param = e match {
      case e if param.isDefinedAt(e) => param(e)
      case e => e
    }
    def rewrite(e: Block): Block = e match {
      case e if block.isDefinedAt(e) => block(e)
      case ScopeAbs(scope, body) =>
        ScopeAbs(scope, rewrite(body))
      case ScopeApp(b, evidence) =>
        ScopeApp(rewrite(b), evidence)
      case Lifted(s, b) =>
        Lifted(s, rewrite(b))
      case BlockLit(params, body) =>
        BlockLit(params map rewrite, rewrite(body))
      case Member(b, field) =>
        Member(rewrite(b), field)
      case Extern(params, body) =>
        Extern(params map rewrite, body)
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
      case e: Expr  => rewrite(e)
      case e: Block => rewrite(e)
    }
  }

  trait Query[T] {
    // Monoid
    def concat(x: T, y: T): T
    def unit: T

    def expr: PartialFunction[Expr, T]
    def stmt: PartialFunction[Stmt, T]
    def param: PartialFunction[Param, T]
    def block: PartialFunction[Block, T]
    def pattern: PartialFunction[Pattern, T]
    def handler: PartialFunction[Handler, T]

    def run(e: Expr): T = e match {
      case e if expr.isDefinedAt(e) => expr(e)
      case e => runGeneric(e)
    }
    def run(e: Stmt): T = e match {
      case e if stmt.isDefinedAt(e) => stmt(e)
      case e => runGeneric(e)
    }
    def run(e: Param): T = e match {
      case e if param.isDefinedAt(e) => param(e)
      case e => runGeneric(e)
    }
    def run(e: Block): T = e match {
      case e if block.isDefinedAt(e) => block(e)
      case e => runGeneric(e)
    }
    def run(e: Pattern): T = e match {
      case e if pattern.isDefinedAt(e) => pattern(e)
      case e => runGeneric(e)
    }
    def run(e: Handler): T = e match {
      case e if handler.isDefinedAt(e) => handler(e)
      case e => runGeneric(e)
    }

    def run(e: Tree): T = e match {
      case e: Expr    => run(e)
      case s: Stmt    => run(s)
      case p: Param   => run(p)
      case b: Block   => run(b)
      case b: Pattern => run(b)
      case b: Handler => run(b)
      case other      => runGeneric(other) // TODO explicitly expand
    }

    def runGeneric(e: Any): T = e match {
      case t: Iterable[t] => t.map(runGeneric).fold(unit)(concat)
      case p: Product => p.productIterator.map {
        case t: Tree => runGeneric(t)
        case other   => unit
      }.fold(unit)(concat)
      case leaf => unit
    }
  }

  trait Visit extends Query[Unit] {
    def concat(x: Unit, y: Unit) = ()

    def expr: PartialFunction[Expr, Unit] = x => ()
    def stmt: PartialFunction[Stmt, Unit] = x => ()
    def param: PartialFunction[Param, Unit] = x => ()
    def block: PartialFunction[Block, Unit] = x => ()
    def pattern: PartialFunction[Pattern, Unit] = x => ()
    def handler: PartialFunction[Handler, Unit] = x => ()
  }
}
