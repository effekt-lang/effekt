package effekt
package cpsds

import core.{ Id, ValueType, BlockType, Captures }
import effekt.source.FeatureFlag
import effekt.util.messages.ErrorReporter
import effekt.util.messages.INTERNAL_ERROR

// Idea behind this IR:
// - close to JS
// - be able to represent continuations and metacontinuations
// - purposefully treat functions, joinpoints and continuations etc the same so the same optimizations apply

sealed trait Tree extends Product {
  /**
   * The number of nodes of this tree (used by inlining heuristics)
   */
  lazy val size: Int = {
    var nodeCount = 1

    def all(t: IterableOnce[_]): Unit = t.iterator.foreach(one)
    def one(obj: Any): Unit = obj match {
      case t: Tree => nodeCount += t.size
      case s: effekt.symbols.Symbol => ()
      case p: Product => all(p.productIterator)
      case t: Iterable[t] => all(t)
      case leaf           => ()
    }
    this.productIterator.foreach(one)
    nodeCount
  }
}

/**
 * A module declaration, the path should be an Effekt include path, not a system dependent file path
 */
case class ModuleDecl(
  path: String,
  includes: List[String],
  declarations: List[core.Declaration],
  externs: List[Extern],
  definitions: List[ToplevelDefinition],
  exports: List[Id]
) extends Tree

enum ToplevelDefinition {
  case Def(id: Id, params: List[Id], body: Stmt)
  case Val(id: Id, binding: Stmt) // this is a let-run
  case Let(id: Id, binding: Expr)
}

/**
 * FFI external definitions
 */
enum Extern extends Tree {
  case Def(id: Id, params: List[Id], async: Boolean, body: ExternBody)
  case Include(featureFlag: FeatureFlag, contents: String)
}
sealed trait ExternBody extends Tree
object ExternBody {
  case class StringExternBody(featureFlag: FeatureFlag, contents: Template[Expr]) extends ExternBody
  case class Unsupported(err: util.messages.EffektError) extends ExternBody {
    def report(using E: ErrorReporter): Unit = E.report(err)
  }
}


enum Expr extends Tree {
  case Variable(id: Id)
  case Literal(value: Any, tpe: core.ValueType)
  case Make(data: ValueType.Data, tag: Id, vargs: List[Expr])

  // Continuations
  case Abort
  case Return

  // MetaContinuations
  case Toplevel
}
export Expr.*

enum Purity {
  case Pure, Impure, Async
}

// just for documentation purposes
type MetaCont = Expr
type Cont = Expr

enum Stmt extends Tree {
  case Def(id: Id, params: List[Id], body: Stmt, rest: Stmt)
  case New(id: Id, interface: BlockType.Interface, operations: List[Operation], rest: Stmt)

  // this is direct style sequencing with the invariant that the binding needs to return using the return continuation
  case Val(id: Id, binding: Stmt, rest: Stmt)

  // also all continuations are named so that we can analyze their usage and jump to them as labels
  case Let(id: Id, binding: Expr, rest: Stmt)
  case App(id: Id, args: List[Expr])
  case Invoke(id: Id, method: Id, args: List[Expr])
  case Run(id: Id, callee: Id, args: List[Expr], purity: Purity, rest: Stmt)

  // Local Control Flow
  case If(cond: Expr, thn: Stmt, els: Stmt)
  case Match(scrutinee: Expr, clauses: List[(Id, Clause)], default: Option[Stmt])

  // Regions
  case Region(id: Id, ks: MetaCont, rest: Stmt)
  case Alloc(id: Id, init: Expr, region: Id, rest: Stmt)

  // creates a fresh state handler to model local (backtrackable) state.
  // [[capture]] is a binding occurence.
  // val id = ks.fresh(init); body
  //
  // Var(x, ..., Toplevel, ...) is used to represent JS-native mutable state
  case Var(id: Id, init: Expr, ks: MetaCont, rest: Stmt)

  // dealloc(ref); rest
  case Dealloc(ref: Id, rest: Stmt)

  // val id = !ref; rest
  case Get(ref: Id, id: Id, rest: Stmt)

  case Put(ref: Id, value: Expr, rest: Stmt)

  // reset( { (p, ks, k) => STMT }, ks1, k1)
  case Reset(p: Id, ks: Id, k: Id, body: Stmt, ks1: MetaCont, k1: Cont)

  // shift(p, { (resume, ks, k) => STMT }, ks1, k1)
  case Shift(prompt: Id, resume: Id, ks: Id, k: Id, body: Stmt, ks1: MetaCont, k1: Cont)

  // resume(r, (ks, k) => STMT, ks1, k1)
  case Resume(resumption: Id, ks: Id, k: Id, body: Stmt, ks1: MetaCont, k1: Cont)

  // Others
  case Hole(span: effekt.source.Span)
}
export Stmt.*

case class Clause(params: List[Id], body: Stmt) extends Tree

case class Operation(name: Id, params: List[Id], body: Stmt) extends Tree


// ---------- Binding Monad ----------

private[cpsds] enum Binding {
  case Let(id: Id, binding: Expr)
  case Def(id: Id, params: List[Id], body: Stmt)
  case New(id: Id, interface: BlockType.Interface, operations: List[Operation])
  case Run(id: Id, callee: Id, args: List[Expr], purity: Purity)

  def toStmt(rest: Stmt): Stmt = this match {
    case Binding.Let(id, binding) => Stmt.Let(id, binding, rest)
    case Binding.Def(id, params, body) => Stmt.Def(id, params, body, rest)
    case Binding.New(id, interface, operations) => Stmt.New(id, interface, operations, rest)
    case Binding.Run(id, callee, args, purity) => Stmt.Run(id, callee, args, purity, rest)
  }
}
private[cpsds] object Binding {
  def apply(bindings: List[Binding], body: Stmt): Stmt = bindings match {
    case Nil => body
    case binding :: rest => binding.toStmt(Binding(rest, body))
  }
}

case class Bind[+A](value: A, bindings: List[Binding]) {
  def run(f: A => Stmt): Stmt = Binding(bindings, f(value))
  def map[B](f: A => B): Bind[B] = Bind(f(value), bindings)
  def flatMap[B](f: A => Bind[B]): Bind[B] =
    val Bind(result, other) = f(value)
    Bind(result, bindings ++ other)
}
object Bind {
  def pure[A](value: A): Bind[A] = Bind(value, Nil)

  def let(expr: Expr): Bind[Expr] =
    val id = Id("tmp")
    Bind(Expr.Variable(id), List(Binding.Let(id, expr)))

  def define(params: List[Id], body: Stmt): Bind[Expr] =
    val id = Id("tmp")
    Bind(Expr.Variable(id), List(Binding.Def(id, params, body)))

  def makeNew(interface: BlockType.Interface, operations: List[Operation]): Bind[Expr] =
    val id = Id("tmp")
    Bind(Expr.Variable(id), List(Binding.New(id, interface, operations)))

  def run(callee: Id, args: List[Expr], purity: Purity): Bind[Expr] =
    val id = Id("tmp")
    Bind(Expr.Variable(id), List(Binding.Run(id, callee, args, purity)))

  def traverse[S, T](l: List[S])(f: S => Bind[T]): Bind[List[T]] =
    l match {
      case Nil => pure(Nil)
      case head :: tail => for { x <- f(head); xs <- traverse(tail)(f) } yield x :: xs
    }
}

object substitutions {

  case class Substitution(
    exprs: Map[Id, Expr] = Map.empty
  ) {
    def shadow(shadowed: IterableOnce[Id]): Substitution =
      copy(exprs = exprs -- shadowed)

    def shadow(id: Id): Substitution =
      copy(exprs = exprs - id)
  }

  def substitute(e: Expr)(using subst: Substitution): Expr = e match {
    case Expr.Variable(id) if subst.exprs.isDefinedAt(id) => subst.exprs(id)
    case Expr.Variable(id) => Expr.Variable(id)
    case Expr.Literal(value, tpe) => Expr.Literal(value, tpe)
    case Expr.Make(data, tag, vargs) => Expr.Make(data, tag, vargs.map(substitute))
    case Expr.Abort => Expr.Abort
    case Expr.Return => Expr.Return
    case Expr.Toplevel => Expr.Toplevel
  }

  def substitute(s: Stmt)(using subst: Substitution): Stmt = s match {
    case Stmt.Def(id, params, body, rest) =>
      Stmt.Def(id, params,
        substitute(body)(using subst.shadow(params)),
        substitute(rest)(using subst.shadow(id)))

    case Stmt.New(id, interface, operations, rest) =>
      Stmt.New(id, interface, operations.map(substitute),
        substitute(rest)(using subst.shadow(id)))

    case Stmt.Val(id, binding, rest) =>
      Stmt.Val(id, substitute(binding),
        substitute(rest)(using subst.shadow(id)))

    case Stmt.Let(id, binding, rest) =>
      Stmt.Let(id, substitute(binding),
        substitute(rest)(using subst.shadow(id)))

    case Stmt.App(id, args) =>
      Stmt.App(substituteAsVar(id), args.map(substitute))

    case Stmt.Invoke(id, method, args) =>
      Stmt.Invoke(substituteAsVar(id), method, args.map(substitute))

    case Stmt.Run(id, callee, args, purity, rest) =>
      Stmt.Run(id, substituteAsVar(callee), args.map(substitute), purity,
        substitute(rest)(using subst.shadow(id)))

    case Stmt.If(cond, thn, els) =>
      Stmt.If(substitute(cond), substitute(thn), substitute(els))

    case Stmt.Match(scrutinee, clauses, default) =>
      Stmt.Match(
        substitute(scrutinee),
        clauses.map { case (id, cl) => (id, substitute(cl)) },
        default.map(substitute))

    case Stmt.Region(id, ks, rest) =>
      Stmt.Region(id, substitute(ks),
        substitute(rest)(using subst.shadow(id)))

    case Stmt.Alloc(id, init, region, rest) =>
      Stmt.Alloc(id, substitute(init), substituteAsVar(region),
        substitute(rest)(using subst.shadow(id)))

    case Stmt.Var(id, init, ks, rest) =>
      Stmt.Var(id, substitute(init), substitute(ks),
        substitute(rest)(using subst.shadow(id)))

    case Stmt.Dealloc(ref, rest) =>
      Stmt.Dealloc(substituteAsVar(ref), substitute(rest))

    case Stmt.Get(ref, id, rest) =>
      Stmt.Get(substituteAsVar(ref), id,
        substitute(rest)(using subst.shadow(id)))

    case Stmt.Put(ref, value, rest) =>
      Stmt.Put(substituteAsVar(ref), substitute(value), substitute(rest))

    case Stmt.Reset(p, ks, k, body, ks1, k1) =>
      Stmt.Reset(p, ks, k,
        substitute(body)(using subst.shadow(List(p, ks, k))),
        substitute(ks1), substitute(k1))

    case Stmt.Shift(prompt, resume, ks, k, body, ks1, k1) =>
      Stmt.Shift(substituteAsVar(prompt), resume, ks, k,
        substitute(body)(using subst.shadow(List(resume, ks, k))),
        substitute(ks1), substitute(k1))

    case Stmt.Resume(r, ks, k, body, ks1, k1) =>
      Stmt.Resume(substituteAsVar(r), ks, k,
        substitute(body)(using subst.shadow(List(ks, k))),
        substitute(ks1), substitute(k1))

    case h: Stmt.Hole => h
  }

  def substitute(op: Operation)(using subst: Substitution): Operation = op match {
    case Operation(name, params, body) =>
      Operation(name, params, substitute(body)(using subst.shadow(params)))
  }

  def substitute(clause: Clause)(using subst: Substitution): Clause = clause match {
    case Clause(params, body) =>
      Clause(params, substitute(body)(using subst.shadow(params)))
  }

  def substituteAsVar(id: Id)(using subst: Substitution): Id =
    subst.exprs.get(id) map {
      case Expr.Variable(x) => x
      case _ => INTERNAL_ERROR("References should always be variables")
    } getOrElse id
}
