package effekt
package cpsds

import core.{ Id, ValueType }
import effekt.source.FeatureFlag
import effekt.util.messages.ErrorReporter
import effekt.util.messages.INTERNAL_ERROR

import scala.annotation.tailrec
import scala.collection.mutable

// Idea behind this IR:
// - close to JS
// - be able to represent continuations and metacontinuations
// - purposefully treat functions, joinpoints and continuations etc the same so the same optimizations apply

sealed trait Tree

/**
 * A module declaration, the path should be an Effekt include path, not a system dependent file path
 */
case class ModuleDecl(
  includes: List[String],
  declarations: List[core.Declaration],
  externs: List[Extern],
  definitions: List[ToplevelDefinition],
  exports: List[Id]
) extends Tree {
  lazy val uses: DB[Set[Id]] = functionUsage.uses(this)
  lazy val escapes: Set[Id] = escapeAnalysis.escapes(this)
  lazy val refs: DB[Int] = references.refs(this)
  lazy val flows: Graph = flowAnalysis.flows(this)
}

enum ToplevelDefinition {
  case Def(id: Id, params: List[Id], body: Stmt)
  case Val(id: Id, ks: Id, k: Id, binding: Stmt)

  lazy val escapes: Set[Id] = escapeAnalysis.escapes(this)
  lazy val free: Set[Id] = freeVariables.free(this)
  lazy val uses: DB[Set[Id]] = functionUsage.uses(this)
  lazy val refs: DB[Int] = references.refs(this)
  lazy val flows: Graph = flowAnalysis.flows(this)
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
  case Make(data: ValueType.Data, tag: Id, args: List[Expr])

  // Continuations
  case Abort
  case Return

  // MetaContinuations
  case Toplevel

  lazy val free: Set[Id] = freeVariables.free(this)
  lazy val refs: DB[Int] = references.refs(this)
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
  case New(id: Id, interface: Id, operations: List[Operation], rest: Stmt)

  // also all continuations are named so that we can analyze their usage and jump to them as labels
  case Let(id: Id, binding: Expr, rest: Stmt)
  case App(id: Id, args: List[Expr], canBeDirect: Boolean)
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

  lazy val free: Set[Id] = freeVariables.free(this)
  lazy val uses: DB[Set[Id]] = functionUsage.uses(this)
  lazy val escapes: Set[Id] = escapeAnalysis.escapes(this)
  lazy val refs: DB[Int] = references.refs(this)
  lazy val calls: DB[Calls] = allCalls.calls(this)
  lazy val info: DB[Info] = definitions.info(this)
  lazy val flows: Graph = flowAnalysis.flows(this)
}
export Stmt.*

case class Clause(params: List[Id], body: Stmt) extends Tree {
  lazy val free: Set[Id] = body.free -- params
  lazy val uses: DB[Set[Id]] = functionUsage.uses(this)
  lazy val escapes: Set[Id] = escapeAnalysis.escapes(this)
  lazy val refs: DB[Int] = references.refs(this)
  lazy val info: DB[Info] = definitions.info(this)
  lazy val calls: DB[Calls] = allCalls.calls(this)
  lazy val flows: Graph = flowAnalysis.flows(this)
}

case class Operation(name: Id, params: List[Id], body: Stmt) extends Tree {
  lazy val free: Set[Id] = body.free -- params
  lazy val uses: DB[Set[Id]] = functionUsage.uses(this)
  lazy val escapes: Set[Id] = escapeAnalysis.escapes(this)
  lazy val refs: DB[Int] = references.refs(this)
  lazy val info: DB[Info] = definitions.info(this)
  lazy val calls: DB[Calls] = allCalls.calls(this)
  lazy val flows: Graph = flowAnalysis.flows(this)
}

inline def rewriting[T <: AnyRef](t: T)(inline run: T => T): T =
  val res = run(t)
  if res == t then t else res


// ---------- Binding Monad ----------

private[cpsds] enum Binding {
  case Let(id: Id, binding: Expr)
  case Def(id: Id, params: List[Id], body: Stmt)
  case New(id: Id, interface: Id, operations: List[Operation])
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

  def makeNew(interface: Id, operations: List[Operation]): Bind[Expr] =
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

  def substitute(e: Expr)(using subst: Substitution): Expr = rewriting(e) {
    case Expr.Variable(id) if subst.exprs.isDefinedAt(id) => subst.exprs(id)
    case Expr.Variable(id) => e
    case Expr.Literal(value, tpe) => e
    case Expr.Make(data, tag, vargs) => Expr.Make(data, tag, vargs.map(substitute))
    case Expr.Abort => e
    case Expr.Return => e
    case Expr.Toplevel => e
  }

  def substitute(s: Stmt)(using subst: Substitution): Stmt = rewriting(s) {
    case Stmt.Def(id, params, body, rest) =>
      Stmt.Def(id, params,
        substitute(body)(using subst.shadow(id :: params)),
        substitute(rest)(using subst.shadow(id)))

    case Stmt.New(id, interface, operations, rest) =>
      Stmt.New(id, interface, operations.map(substitute),
        substitute(rest)(using subst.shadow(id)))

    case Stmt.Let(id, binding, rest) =>
      Stmt.Let(id, substitute(binding),
        substitute(rest)(using subst.shadow(id)))

    case Stmt.App(id, args, direct) =>
      Stmt.App(substituteAsVar(id), args.map(substitute), direct)

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

  def substitute(op: Operation)(using subst: Substitution): Operation = rewriting(op) {
    case Operation(name, params, body) =>
      Operation(name, params, substitute(body)(using subst.shadow(params)))
  }

  def substitute(clause: Clause)(using subst: Substitution): Clause = rewriting(clause) {
    case Clause(params, body) =>
      Clause(params, substitute(body)(using subst.shadow(params)))
  }

  def substituteAsVar(id: Id)(using subst: Substitution): Id =
    subst.exprs.get(id) map {
      case Expr.Variable(x) => x
      case _ => INTERNAL_ERROR("References should always be variables")
    } getOrElse id
}

/**
 * Free variables
 *
 * Implementation notes:
 * - we use .free where possible in order to use the already computed result
 *   cached on the node itself.
 * - we mark everything as inline to have the Scala compiler check that we do not
 *   miss any caches (since inlines cannot be recursive).
 */
object freeVariables {

  private inline def all[T](t: IterableOnce[T], inline f: T => Set[Id]): Set[Id] =
    t.iterator.foldLeft(Set.empty) { case (xs, t) => f(t) ++ xs }

  private inline def free(id: Id): Set[Id] = Set(id)
  private inline def bound(ids: List[Id]): Set[Id] = ids.toSet
  private val closed = Set.empty[Id]

  inline def free(e: Expr): Set[Id] = e match {
    case Expr.Variable(id) => free(id)
    case Expr.Literal(_, _) => closed
    case Expr.Make(_, _, vargs) => all(vargs, _.free)
    case Expr.Abort => closed
    case Expr.Return => closed
    case Expr.Toplevel => closed
  }

  inline def free(op: Operation): Set[Id] = op match {
    case Operation(name, params, body) => body.free -- bound(params)
  }

  inline def free(cl: Clause): Set[Id] = cl match {
    case Clause(params, body) => body.free -- bound(params)
  }

  inline def free(toplevel: ToplevelDefinition): Set[Id] = toplevel match {
    case ToplevelDefinition.Def(id, params, body) => body.free -- bound(params) - id
    case ToplevelDefinition.Val(id, ks, k, binding) => binding.free - ks - k
  }

  inline def free(s: Stmt): Set[Id] = s match {
    case Stmt.Def(id, params, body, rest) =>
      (body.free -- bound(params) - id) ++ (rest.free - id)

    case Stmt.New(id, _, operations, rest) =>
      all(operations, _.free) ++ (rest.free - id)

    case Stmt.Let(id, binding, rest) =>
      binding.free ++ (rest.free - id)

    case Stmt.App(id, args, direct) =>
      free(id) ++ all(args, _.free)

    case Stmt.Invoke(id, _, args) =>
      free(id) ++ all(args, _.free)

    case Stmt.Run(id, callee, args, _, rest) =>
      free(callee) ++ all(args, _.free) ++ (rest.free - id)

    case Stmt.If(cond, thn, els) =>
      cond.free ++ thn.free ++ els.free

    case Stmt.Match(scrutinee, clauses, default) =>
      scrutinee.free ++ all(clauses, { case (id, cl) => cl.free }) ++ all(default, _.free)

    case Stmt.Region(id, ks, rest) =>
      ks.free ++ (rest.free - id)

    case Stmt.Alloc(id, init, region, rest) =>
      init.free ++ free(region) ++ (rest.free - id)

    case Stmt.Var(id, init, ks, rest) =>
      init.free ++ ks.free ++ (rest.free - id)

    case Stmt.Dealloc(ref, rest) =>
      free(ref) ++ rest.free

    case Stmt.Get(ref, id, rest) =>
      free(ref) ++ (rest.free - id)

    case Stmt.Put(ref, value, rest) =>
      free(ref) ++ value.free ++ rest.free

    case Stmt.Reset(p, ks, k, body, ks1, k1) =>
      (body.free - p - ks - k) ++ ks1.free ++ k1.free

    case Stmt.Shift(prompt, resume, ks, k, body, ks1, k1) =>
      free(prompt) ++ (body.free - resume - ks - k) ++ ks1.free ++ k1.free

    case Stmt.Resume(r, ks, k, body, ks1, k1) =>
      free(r) ++ (body.free - ks - k) ++ ks1.free ++ k1.free

    case Stmt.Hole(_) => closed
  }
}

/**
 * Which function refers to which other function?
 *
 * Small example:
 *    def f() { g(h) }; ...
 *
 * results in
 *    f -> {g, h}
 *
 * Is computed at every **definition** based on the free variables.
 * On the toplevel, a fixed point computation is necessary due to mutual recursion.
 *
 * The keyset at the toplevel also gives a simple way to get a list of all function
 * definitions (only their names).
 *
 * It also provides the basis to determine whether functions are (mutually) recursive.
 *
 * Larger example:
 *    def outer(x, ks, k) {
 *      run tmp = eq(x, 0);
 *      if (tmp) {
 *        k(0, ks)
 *      } else {
 *        run tmp = sub(x, 1);
 *        outer(tmp, ks, k)
 *      }
 *     }
 *     def main(ks, k) {
 *       def k1(res, ks) {
 *         outer(res, ks, k)
 *       }
 *       outer(10, ks, k1)
 *     }
 *
 * results in
 *   outer -> {outer}
 *   k1 -> {outer}
 *   main -> {outer}
 */
object functionUsage {
  extension (db: DB[Set[Id]]) {
    // since every function should be only added ONCE we can simply concatenate the DBs
    private def ++(other: DB[Set[Id]]): DB[Set[Id]] = db.unionWith(other, (l, r) => r)
  }

  private inline def all[T](t: Iterable[T], inline f: T => DB[Set[Id]]): DB[Set[Id]] =
    t.foldLeft(DB.empty[Set[Id]]) { case (acc, t) => acc ++ f(t) }

  inline def uses(cl: Clause): DB[Set[Id]] = uses(cl.body)

  inline def uses(op: Operation): DB[Set[Id]] = uses(op.body)

  inline def uses(stmt: Stmt): DB[Set[Id]] = stmt match {
    case Stmt.Def(id, params, body, rest) =>
      rest.uses + (id -> (body.free -- params))
    case Stmt.New(id, interface, operations, rest) =>
      val freeInOperations = operations.foldLeft(rest.free) { case (acc, op) => acc ++ op.free }
      rest.uses ++ all(operations, _.uses) + (id -> freeInOperations)
    case Stmt.Let(id, binding, rest) =>
      rest.uses
    case Stmt.App(id, args, canBeDirect) =>
      DB.empty
    case Stmt.Invoke(id, method, args) =>
      DB.empty
    case Stmt.Run(id, callee, args, purity, rest) =>
      rest.uses
    case Stmt.If(cond, thn, els) =>
      thn.uses ++ els.uses
    case Stmt.Match(scrutinee, clauses, default) =>
      all(clauses, { case (_, clause) => clause.uses }) ++ all(default, _.uses)
    case Stmt.Region(id, ks, rest) =>
      rest.uses
    case Stmt.Alloc(id, init, region, rest) =>
      rest.uses
    case Stmt.Var(id, init, ks, rest) =>
      rest.uses
    case Stmt.Dealloc(ref, rest) =>
      rest.uses
    case Stmt.Get(ref, id, rest) =>
      rest.uses
    case Stmt.Put(ref, value, rest) =>
      rest.uses
    case Stmt.Reset(p, ks, k, body, ks1, k1) =>
      body.uses
    case Stmt.Shift(prompt, resume, ks, k, body, ks1, k1) =>
      body.uses
    case Stmt.Resume(resumption, ks, k, body, ks1, k1) =>
      body.uses
    case Stmt.Hole(span) =>
      DB.empty
  }

  // Warning: this info is not accurate since we need to compute the fixed point on the toplevel
  inline def uses(toplevel: ToplevelDefinition): DB[Set[Id]] = toplevel match {
    case ToplevelDefinition.Def(id, params, body) =>
      body.uses + (id -> (body.free -- params))
    case ToplevelDefinition.Val(id, ks, k, binding) =>
      binding.uses
  }

  def uses(m: ModuleDecl): DB[Set[Id]] = {
    val toplevelIds = m.definitions.collect {
      case ToplevelDefinition.Def(id, _, _) => id
    }.toSet

    // Collect uses from all toplevel definitions
    val allUses: DB[Set[Id]] = all(m.definitions, _.uses)
    val definitions: Set[Id] = allUses.keys

    // Filter to only keep references that are definitions themselves
    // TODO figure out a cheaper way to do this
    val filtered: DB[Set[Id]] = allUses.mapValues { ids => ids.intersect(definitions) }

    // Compute transitive closure for toplevel Defs only
    var current = filtered
    var changed = true
    while (changed) {
      changed = false
      current = current.mapWithId { case (id, refs) =>
        if (!toplevelIds.contains(id)) refs
        else {
          val expanded = refs.foldLeft(refs) { case (acc, r) =>
            acc ++ current.getOrElse(r, Set.empty[Id])
          }
          if (expanded.size != refs.size) changed = true
          expanded
        }
      }
    }
    current
  }
}

/**
 * Analyses which functions escape
 *
 * Implementation details:
 * - for simplicity we collect ALL escaping variables, not just functions
 */
object escapeAnalysis {

  inline def escapes(m: ModuleDecl): Set[Id] = m match {
    case ModuleDecl(includes, declarations, externs, definitions, exports) =>
      definitions.flatMap(escapes).toSet
  }

  // All free variables of an object escape
  inline def escapes(op: Operation): Set[Id] = op match {
    case Operation(name, params, body) =>
      body.escapes ++ (body.free -- params)
  }

  inline def escapes(cl: Clause): Set[Id] = cl match {
    case Clause(params, body) => body.escapes
  }

  inline def escapes(stmt: Stmt): Set[Id] = stmt match {

    // Free variables of a def escape if the definition itself
    // escapes.
    case Stmt.Def(id, params, body, rest) =>
      val both = body.escapes ++ rest.escapes
      if (both contains id) {
        (body.free -- params) ++ both
      } else both

    case Stmt.New(id, interface, operations, rest) =>
      operations.foldLeft(rest.escapes) { case (acc, op) => acc ++ op.escapes }

    case Stmt.Let(id, binding, rest) =>
      rest.escapes ++ binding.free

    // callee does NOT escape
    case Stmt.App(id, args, canBeDirect) =>
      args.flatMap(_.free).toSet

    case Stmt.Invoke(id, method, args) =>
      args.flatMap(_.free).toSet

    // This is the essence of async computation: we need to reify the continuation
    case Stmt.Run(id, callee, args, Purity.Async, rest) =>
      rest.free ++ rest.escapes ++ args.flatMap(_.free)

    case Stmt.Run(id, callee, args, purity, rest) =>
      rest.escapes ++ args.flatMap(_.free)

    case Stmt.If(cond, thn, els) =>
      cond.free ++ thn.escapes ++ els.escapes

    case Stmt.Match(scrutinee, clauses, default) =>
      scrutinee.free ++ clauses.flatMap { case (id, cl) => cl.escapes } ++ default.map(_.escapes).getOrElse(Set.empty)

    case Stmt.Region(id, ks, rest) => rest.escapes
    case Stmt.Alloc(id, init, region, rest) => init.free ++ rest.escapes
    case Stmt.Var(id, init, ks, rest) => init.free ++ rest.escapes
    case Stmt.Dealloc(ref, rest) => rest.escapes
    case Stmt.Get(ref, id, rest) => rest.escapes
    case Stmt.Put(ref, value, rest) => value.free ++ rest.escapes

    case Stmt.Reset(p, ks, k, body, ks1, k1) =>
      ks1.free ++ k1.free ++ body.escapes ++ body.free

    case Stmt.Shift(prompt, resume, ks, k, body, ks1, k1) =>
      ks1.free ++ k1.free ++ body.escapes ++ body.free

    case Stmt.Resume(resumption, ks, k, body, ks1, k1) =>
      ks1.free ++ k1.free ++ body.escapes ++ body.free

    case Stmt.Hole(span) => Set.empty
  }

  inline def escapes(toplevel: ToplevelDefinition): Set[Id] = toplevel match {
    // We mark toplevel definitions as escaping
    case ToplevelDefinition.Def(id, params, body) =>
      Set(id) ++ body.escapes
    case ToplevelDefinition.Val(id, ks, k, binding) =>
      binding.escapes
  }
}

// TODO better name for this?
trait UseDB[T] {

  def merge(t1: T, t2: T): T

  def empty: DB[T] = DB.empty[T]

  extension (db: DB[T]) {
    def ++(other: DB[T]): DB[T] = db.unionWith(other, merge)
  }

  inline def all[A](terms: Iterable[A], inline run: A => DB[T]): DB[T] =
    terms.foldLeft(DB.empty[T]) { (acc, t) => acc ++ run(t) }
}

/**
 * How often is something (a function or expression) referenced in a subterm?
 */
object references extends UseDB[Int] {

  def merge(t1: Int, t2: Int): Int = t1 + t2

  private inline def use(id: Id): DB[Int] = DB(id, 1)

  // TODO externs and splices
  inline def refs(m: ModuleDecl): DB[Int] = m match {
    case ModuleDecl(includes, declarations, externs, definitions, exports) =>
      all(definitions, _.refs)
  }

  inline def refs(t: ToplevelDefinition): DB[Int] = t match {
    case ToplevelDefinition.Def(id, params, body) => body.refs
    case ToplevelDefinition.Val(id, ks, k, binding) => binding.refs
  }

  inline def refs(expr: Expr): DB[Int] = expr match {
    case Expr.Variable(id) => use(id)
    case Expr.Literal(value, tpe) => empty
    case Expr.Make(data, tag, args) => all(args, _.refs)
    case Expr.Abort => empty
    case Expr.Return => empty
    case Expr.Toplevel => empty
  }

  inline def refs(stmt: Stmt): DB[Int] = stmt match {
    case Stmt.App(id, args, canBeDirect) => use(id) ++ all(args, _.refs)
    case Stmt.Invoke(id, method, args) => use(id) ++ all(args, _.refs)
    case Stmt.Alloc(id, init, region, rest) => use(region) ++ init.refs ++ rest.refs
    case Stmt.Dealloc(ref, rest) => use(ref) ++ rest.refs
    case Stmt.Get(ref, id, rest) => use(ref) ++ rest.refs
    case Stmt.Put(ref, value, rest) => use(ref) ++ value.refs ++ rest.refs
    case Stmt.Shift(prompt, resume, ks, k, body, ks1, k1) =>
      use(prompt) ++ body.refs ++ ks1.refs ++ k1.refs
    case Stmt.Resume(resumption, ks, k, body, ks1, k1) =>
      use(resumption) ++ body.refs ++ ks1.refs ++ k1.refs

    case Stmt.Def(id, params, body, rest) => body.refs ++ rest.refs
    case Stmt.New(id, interface, operations, rest) => all(operations, _.refs) ++ rest.refs
    case Stmt.Let(id, binding, rest) => binding.refs ++ rest.refs
    case Stmt.Run(id, callee, args, purity, rest) => all(args, _.refs) ++ rest.refs
    case Stmt.If(cond, thn, els) => cond.refs ++ thn.refs ++ els.refs
    case Stmt.Match(scrutinee, clauses, default) =>
      scrutinee.refs ++ all(clauses, { case (tag, cl) => cl.refs }) ++ all(default, _.refs)
    case Stmt.Region(id, ks, rest) => ks.refs ++ rest.refs
    case Stmt.Var(id, init, ks, rest) => init.refs ++ ks.refs ++ rest.refs
    case Stmt.Reset(p, ks, k, body, ks1, k1) => body.refs ++ ks1.refs ++ k1.refs
    case Stmt.Hole(span) => DB.empty
  }

  inline def refs(cl: Clause): DB[Int] = cl.body.refs
  inline def refs(op: Operation): DB[Int] = op.body.refs
}

//   def f(x, h) {
//     g(x, 1)
//     g(x, 4)
//     h(3)
//   }
// results in
//   g -> ({x}, {1, 4})
//   h -> ({3})
type Calls = Vector[List[Expr]]
object allCalls extends UseDB[Calls] {

  def merge(t1: Calls, t2: Calls): Calls = t1 ++ t2

  private inline def call(id: Id, args: List[Expr]): DB[Calls] =
    DB(id, Vector(args))

  inline def calls(op: Operation): DB[Calls] = op.body.calls
  inline def calls(cl: Clause): DB[Calls] = cl.body.calls

  inline def calls(stmt: Stmt): DB[Calls] =
    println(s"calls on ${stmt.hashCode}")
    stmt match {
    case Stmt.App(id, args, canBeDirect) => call(id, args)
    case Stmt.Invoke(id, method, args) => empty

    case Stmt.Def(id, params, body, rest) => body.calls ++ rest.calls
    case Stmt.New(id, interface, operations, rest) => all(operations, _.calls) ++ rest.calls
    case Stmt.Let(id, binding, rest) => rest.calls

    case Stmt.Run(id, callee, args, purity, rest) => rest.calls
    case Stmt.If(cond, thn, els) => thn.calls ++ els.calls
    case Stmt.Match(scrutinee, clauses, default) => all(clauses, (tag, cl) => cl.calls) ++ all(default, _.calls)
    case Stmt.Region(id, ks, rest) => rest.calls
    case Stmt.Alloc(id, init, region, rest) => rest.calls
    case Stmt.Var(id, init, ks, rest) => rest.calls
    case Stmt.Dealloc(ref, rest) => rest.calls
    case Stmt.Get(ref, id, rest) => rest.calls
    case Stmt.Put(ref, value, rest) => rest.calls
    case Stmt.Reset(p, ks, k, body, ks1, k1) => body.calls
    case Stmt.Shift(prompt, resume, ks, k, body, ks1, k1) => body.calls
    case Stmt.Resume(resumption, ks, k, body, ks1, k1) => body.calls
    case Stmt.Hole(span) => empty
  }
}

enum Info {
  case Parameter(calls: Calls)
  case Function(
    params: List[Id],
    body: Stmt,
    internal: Calls,
    external: Calls
  )
}

//   def f(x, y) {
//     g(x, 1)
//     g(x, 4)
//   }
// results in
//   f -> Def(params, body)
// could be extended to values as well for other analyses
//
// - for now only functions not objects
// - for now only for parameters of functions, not parameters of
//   builtins like shift (which could in the future be used to optimize further)
//
// TODO what if the flow is indirect such as
//
//  def outer(f) {
//    let x = f
//    x(1)
//  }
//
// this would look like f is not called
object definitions extends UseDB[Info] {

  def merge(l: Info, r: Info): Info = (l, r) match {
    case (Info.Parameter(calls1), Info.Parameter(calls2)) => Info.Parameter(calls1 ++ calls2)
    case (Info.Function(params1, body1, internal1, external1), Info.Function(params2, body2, internal2, external2)) =>
      Info.Function(params1, body1, internal1 ++ internal2, external1 ++ external2)
    case _ => ???
  }

  inline def info(op: Operation): DB[Info] = op match {
    case Operation(name, params, body) => body.info
  }

  inline def info(op: Clause): DB[Info] = op match {
    case Clause(params, body) => body.info
  }

  inline def info(stmt: Stmt): DB[Info] =
    println(s"info on ${stmt.hashCode}")
    stmt match {
    case Stmt.Def(id, params, body, rest) =>
      val funInfo = DB(id, Info.Function(params, body,
        body.calls.getOrElse(id, Vector.empty),
        rest.calls.getOrElse(id, Vector.empty)))

      val paramsInfo = all(params, p =>
        DB(p, Info.Parameter(body.calls.getOrElse(p, Vector.empty)))
      )

      funInfo ++ paramsInfo ++ rest.info

    case Stmt.New(id, interface, operations, rest) => all(operations, _.info) ++ rest.info
    case Stmt.If(cond, thn, els) => thn.info ++ els.info
    case Stmt.Match(scrutinee, clauses, default) => all(clauses, (tag, cl) => cl.info) ++ all(default, _.info)

    case Stmt.Let(id, binding, rest) => rest.info
    case Stmt.App(id, args, canBeDirect) => DB.empty
    case Stmt.Invoke(id, method, args) => DB.empty
    case Stmt.Run(id, callee, args, purity, rest) => rest.info
    case Stmt.Region(id, ks, rest) => rest.info
    case Stmt.Alloc(id, init, region, rest) => rest.info
    case Stmt.Var(id, init, ks, rest) => rest.info
    case Stmt.Dealloc(ref, rest) => rest.info
    case Stmt.Get(ref, id, rest) => rest.info
    case Stmt.Put(ref, value, rest) => rest.info
    case Stmt.Reset(p, ks, k, body, ks1, k1) => body.info
    case Stmt.Shift(prompt, resume, ks, k, body, ks1, k1) => body.info
    case Stmt.Resume(resumption, ks, k, body, ks1, k1) => body.info
    case Stmt.Hole(span) => DB.empty
  }
}

// TODO right now we track the individual flows
//   it might be MUCH better to saturate the flow graph locally and "cache" the partial solution
//   for instance:
//     - flows 1 <: ?   can be dropped
//     - storing it in a form that keeps upper / lower bounds for ids might make merging / saturating more efficient
//     - conceptually we can draw the flows as a graph,
//       each subtree gives rise to a subgraph and composing them connects the graphs.
//
// On the other side: the fixedpoint analysis might be more efficient if run in a single tight loop that
// uses mutable state, so we really should benchmark this before making a decision.

enum UnknownFlow {
  // f(y, ...)  .=   y <: f(0)
  case Call(from: Expr, to: Id, index: Int)
  // x <: y   or   42  <:  y
  case Data(from: Expr, to: Id)
}

enum KnownFlow {
  // y <: f(0)
  case Call(from: Expr, to: Id, index: Int)
  // 42 <: x
  case Data(from: Expr, to: Id)
  // x <: ?
  case Sink(from: Id)
}


case class Signature(id: Id, params: List[Id])

case class Graph(
  // the signatures of all functions encountered so far
  functions: Vector[Signature],
  // flows into parameters and variables not encountered so far
  unknownFlows: Set[UnknownFlow],
  // flows into parameters and variables that are bound
  knownFlows: Set[KnownFlow]
) {
  def ++(other: Graph): Graph =
    Graph(functions ++ other.functions, unknownFlows ++ other.unknownFlows, knownFlows ++ other.knownFlows)

  def toDot: String = {
    val sb = new StringBuilder
    sb ++= "digraph FlowGraph {\n"
    sb ++= "  rankdir=TB;\n"
    sb ++= "  node [fontname=\"Helvetica\"];\n"
    sb ++= "  edge [fontname=\"Helvetica\"];\n\n"

    var counter = 0
    def freshNode(): String = { counter += 1; s"n$counter" }

    def nodeId(id: Id): String = s"id_${id.hashCode.abs}"

    val functionIds: Set[Id] = functions.map(_.id).toSet

    val paramOwner: Map[Id, (Id, String)] = {
      val result = scala.collection.mutable.Map.empty[Id, (Id, String)]
      functions.foreach { case Signature(id, params) =>
        params.foreach { p =>
          if (!result.contains(p)) {
            result(p) = (id, s"p_${p.hashCode.abs}")
          }
        }
      }
      result.toMap
    }

    val knownIds: Set[Id] = functionIds ++ paramOwner.keySet
    val declaredIds = scala.collection.mutable.Set.empty[Id]

    def targetRef(id: Id): String =
      if (functionIds.contains(id)) nodeId(id)
      else paramOwner.get(id) match {
        case Some((funId, port)) => s"${nodeId(funId)}:$port"
        case None =>
          if (declaredIds.add(id)) {
            sb ++= s"  ${nodeId(id)} [label=\"${util.show(id)}\"];\n"
          }
          nodeId(id)
      }

    // Function nodes as HTML tables
    functions.foreach { case Signature(id, params) =>
      val funLabel = util.show(id)
      val paramCells = params.map { p =>
        val port = paramOwner(p)._2
        s"""<TD PORT="$port" BORDER="1" STYLE="ROUNDED"> ${util.show(p)} </TD>"""
      }.mkString("")

      sb ++= s"  ${nodeId(id)} [shape=plain, label=<"
      sb ++= s"""<TABLE BORDER="0" CELLSPACING="4" CELLPADDING="4">"""
      sb ++= s"<TR>"
      sb ++= s"""<TD BORDER="1"><B>${funLabel}</B></TD>"""
      sb ++= paramCells
      sb ++= s"</TR></TABLE>"
      sb ++= s">];\n\n"
    }

    // Vertical ordering
    if (functions.size > 1) {
      val funChain = functions.map(s => nodeId(s.id)).mkString(" -> ")
      sb ++= s"  $funChain [style=invis];\n\n"
    }

    def sourceNode(from: Expr): String = from match {
      case Expr.Variable(id) if knownIds.contains(id) => targetRef(id)
      case other =>
        val n = freshNode()
        sb ++= s"  $n [shape=plain, label=\"${util.show(other)}\"];\n"
        n
    }

    knownFlows.foreach {
      case KnownFlow.Data(from, to) =>
        sb ++= s"  ${sourceNode(from)} -> ${targetRef(to)};\n"

      case KnownFlow.Sink(from) =>
        val sinkNode = freshNode()
        sb ++= s"  $sinkNode [shape=plain, label=\"?\"];\n"
        sb ++= s"  ${targetRef(from)} -> $sinkNode;\n"

      case KnownFlow.Call(from, to, index) =>
        sb ++= s"  ${sourceNode(from)} -> ${targetRef(to)} [label=\"@$index\", style=dashed];\n"
    }

    unknownFlows.foreach {
      case UnknownFlow.Call(from, to, index) =>
        sb ++= s"  ${sourceNode(from)} -> ${targetRef(to)} [label=\"@$index\", style=dashed];\n"

      case UnknownFlow.Data(from, to) =>
        sb ++= s"  ${sourceNode(from)} -> ${targetRef(to)} [style=dotted];\n"
    }

    sb ++= "}\n"
    sb.toString
  }

  def dump(filename: String = "graph"): Unit = {
    import java.nio.file.{Files, Paths}
    val outDir = Paths.get("out")
    Files.createDirectories(outDir)
    val dot = toDot
    val dotPath = outDir.resolve(s"$filename.dot")
    val pngPath = outDir.resolve(s"$filename.png")
    Files.writeString(dotPath, dot)
    import scala.sys.process._
    s"dot -Tpng $dotPath -o $pngPath".!
  }

  def show: String = ""

}
object Graph {
  val empty = Graph(Vector.empty, Set.empty, Set.empty)

  def define(id: Id, params: List[Id], body: Graph, rest: Graph): Graph =
    var newUnknown: Set[UnknownFlow] = Set.empty
    var newKnown: Set[KnownFlow] = body.knownFlows ++ rest.knownFlows


    def emitData(from: Expr, to: Id): Unit = from match {
      case Expr.Variable(other) if to == other => ()
      case other => newKnown += KnownFlow.Data(from, to)
    }

    def emitCall(from: Expr, to: Id, index: Int): Unit =
      newKnown += KnownFlow.Call(from, to, index)

    rest.unknownFlows.foreach {
      // def f(x, y) {}
      // expr <: f(0)  ~>  expr <: x
      case UnknownFlow.Call(from, to, index) if to == id => emitData(from, params(index))
      case other => newUnknown += other
    }

    body.unknownFlows.foreach {
      case UnknownFlow.Call(from, to, index) if to == id => emitData(from, params(index))
      case UnknownFlow.Call(from, to, index) if params.contains(to) => emitCall(from, to, index)
      case UnknownFlow.Data(from, to) if to == id => emitData(from, to)
      case other => newUnknown += other
    }
    Graph(Signature(id, params) +: (body.functions ++ rest.functions), newUnknown, newKnown)

  def bind(id: Id, body: Graph): Graph = {
    var newUnknown: Set[UnknownFlow] = Set.empty
    var newKnown: Set[KnownFlow] = body.knownFlows

    body.unknownFlows.foreach {
      case UnknownFlow.Call(from, to, index) if to == id => newKnown += KnownFlow.Call(from, to, index)
      case UnknownFlow.Data(from, to) if to == id => newKnown += KnownFlow.Data(from, to)
      case other => newUnknown += other
    }
    Graph(body.functions, newUnknown, newKnown)
  }

  // all unknown flows are now sinks
  def close(graph: Graph): Graph =
    var knownFlows = graph.knownFlows
    graph.unknownFlows.foreach {
      case UnknownFlow.Call(from, to, index) => knownFlows ++= from.free.map(x => KnownFlow.Sink(x))
      case UnknownFlow.Data(from, to) => knownFlows ++= from.free.map(x => KnownFlow.Sink(x))
    }
    Graph(graph.functions, Set.empty, knownFlows)
}

object flowAnalysis {

  private inline def all[T](t: IterableOnce[T], inline f: T => Graph): Graph =
    t.iterator.foldLeft(Graph.empty) { case (xs, t) => f(t) ++ xs }

  private def sink(e: Expr): Graph =
    e match {
      case _: Expr.Literal | Expr.Abort | Expr.Return | Expr.Toplevel  => Graph.empty
      case other =>
        Graph(Vector.empty, Set.empty, other.free.map(id => KnownFlow.Sink(id)))
    }

  private def sink(id: Id): Graph = Graph(Vector.empty, Set.empty, Set(KnownFlow.Sink(id)))

  private def known(from: Expr, to: Id): Graph =
    Graph(Vector.empty, Set.empty, Set(KnownFlow.Data(from, to)))

  inline def flows(stmt: Stmt): Graph = stmt match {
    case Stmt.App(id, args, canBeDirect) =>
      // at first all functions are "unknown", we resolve them bottom up and refine the graph
      val unknownFlows = args.zipWithIndex.map { case (arg, index) => UnknownFlow.Call(arg, id, index) }.toSet
      Graph(Vector.empty, unknownFlows, Set.empty)

    // for now this is treated as a sink
    case Stmt.Invoke(id, method, args) => all(args, sink)
    case Stmt.Run(id, callee, args, purity, rest) => all(args, sink) ++ rest.flows
    case Stmt.If(cond, thn, els) => sink(cond) ++ thn.flows ++ els.flows
    case Stmt.Let(id, binding, rest) =>
      known(binding, id) ++ Graph.bind(id, rest.flows)

    case Stmt.Def(id, params, body, rest) =>
      Graph.define(id, params, body.flows, rest.flows)

    case Stmt.New(id, interface, operations, rest) => all(operations, _.flows) ++ rest.flows
    case Stmt.Match(scrutinee, clauses, default) => all(clauses, (tag, cl) => cl.flows) ++ all(default, _.flows)
    case Stmt.Region(id, ks, rest) => sink(ks) ++ rest.flows
    // TODO region?
    case Stmt.Alloc(id, init, region, rest) => sink(init) ++ sink(region) ++ rest.flows
    case Stmt.Var(id, init, ks, rest) => sink(init) ++ sink(ks) ++ rest.flows
    case Stmt.Dealloc(ref, rest) => rest.flows
    case Stmt.Get(ref, id, rest) => sink(ref) ++ rest.flows
    case Stmt.Put(ref, value, rest) => sink(ref) ++ sink(value) ++ rest.flows
    // if we want to track flows to resets and shifts, we also need to have labels for the positions
    // for now it is just unknown "sinks"
    // we could also "mask" variables that we do not care about, like p, ks, and k for now
    // potentially simplifying the flow graph (both for inspection and performance)
    case Stmt.Reset(p, ks, k, body, ks1, k1) =>
      body.flows ++ sink(ks1) ++ sink(k1)
    case Stmt.Shift(prompt, resume, ks, k, body, ks1, k1) =>
      sink(prompt) ++ body.flows ++ sink(ks1) ++ sink(k1)
    case Stmt.Resume(resumption, ks, k, body, ks1, k1) =>
      sink(resumption) ++ body.flows ++ sink(ks1) ++ sink(k1)
    case Stmt.Hole(span) => Graph.empty
  }

  inline def flows(op: Operation): Graph = op.body.flows
  inline def flows(cl: Clause): Graph = cl.body.flows

  def flows(toplevel: ToplevelDefinition): Graph = toplevel match {
    case ToplevelDefinition.Def(id, params, body) =>
      val graph = Graph.define(id, params, body.flows, Graph.empty)
      Graph.close(graph)

    case ToplevelDefinition.Val(id, ks, k, binding) => binding.flows
  }

  def solve(toplevel: ToplevelDefinition, main: Id): Map[Id, NodeInfo] = toplevel match {
    case ToplevelDefinition.Def(id, params, body) =>
      val flows = toplevel.flows
      solve(flows.functions, flows.knownFlows, params.toSet, main)
    case ToplevelDefinition.Val(id, ks, k, binding) => Map.empty
  }
  inline def flows(m: ModuleDecl): Graph = m match {
    case ModuleDecl(includes, declarations, externs, definitions, exports) =>
      val allFlows = all(definitions, _.flows)
      assert(allFlows.unknownFlows.isEmpty)
      allFlows
  }

  enum Usage {
    case Unused, SecondClass, FirstClass

    def join(other: Usage): Usage = (this, other) match {
      case (FirstClass, _) | (_, FirstClass) => FirstClass
      case (SecondClass, _) | (_, SecondClass) => SecondClass
      case _ => Unused
    }
    override def toString: String = this match {
      case Usage.Unused => "–"
      case Usage.SecondClass => "2nd"
      case Usage.FirstClass => "1st"
    }
  }

  case class NodeInfo(
    inputs: Set[Expr],
    usage: Usage
  )

  extension (result: Map[Id, NodeInfo]) {
    def show: String = {
      val entries = result.toList
        .sortBy((id, _) => util.show(id))
        .map { case (id, NodeInfo(inputs, usage)) =>
          val inputsStr = "{" + inputs.map(util.show).mkString(", ") + "}"
          val idStr = util.show(id)
          val usageStr = usage.toString
          (inputsStr, idStr, usageStr)
        }

      val maxInputs = entries.map(_._1.length).maxOption.getOrElse(0)
      val maxId = entries.map(_._2.length).maxOption.getOrElse(0)

      entries.map { case (inputsStr, idStr, usageStr) =>
        s"${inputsStr.padTo(maxInputs, ' ')} <: ${idStr.padTo(maxId, ' ')} <: $usageStr"
      }.mkString("\n")
    }
  }

  /**
   * Solves the flow graph to determine for each node:
   *   - inputs: the set of expressions that flow into it (after propagation)
   *   - usage: how it is used (Unused / SecondClass / FirstClass)
   *
   * The algorithm has three phases:
   *
   * Phase 1: Extract static inputs, calls, and sinks from the flow edges.
   *
   * Phase 2: Propagate inputs forward (fixpoint).
   *   - If a node has a single input, it is "transparent": wherever Variable(node)
   *     appears as an input to another node, it is replaced by that single input.
   *     If a node has multiple inputs, it is a "join point" and remains opaque.
   *   - Self-references (from recursive flows) are dropped.
   *   - Calls are resolved when all callee inputs are known functions —
   *     arguments flow into each function's corresponding parameter.
   *   - Unresolved calls (callee has unknown inputs) mark their arguments as
   *     escaping (FirstClass).
   *
   * Phase 3: Propagate usage backward (fixpoint).
   *   - Sinks: FirstClass (used in constructs like make, or escaping arguments)
   *   - Toplevel params: FirstClass
   *   - Callees in resolved calls: SecondClass (all call sites known)
   *   - Callees in unresolved calls: FirstClass (call site not fully controllable)
   *   - Usage propagates backward along flow edges (recorded before substitution,
   *     so transparent nodes still participate).
   *   - Join is max: FirstClass > SecondClass > Unused
   *
   * Parameter dropping is safe when:
   *   - The parameter has a single input (can be specialized), AND
   *   - The owning function is at most SecondClass (all call sites are known)
   * If a function is FirstClass but has specializable parameters, the
   * transformation pass can eta-expand at first-class use sites.
   */
  def solve(
    signatures: Vector[Signature],
    flows: Set[KnownFlow],
    toplevelParams: Set[Id],
    main: Id
  ): Map[Id, NodeInfo] = {

    val knownFunctions: Map[Id, Signature] = signatures.map(s => s.id -> s).toMap

    // --- Phase 1: Build initial graph structure ---

    val dynamicInputs = scala.collection.mutable.Map.empty[Id, Set[Expr]].withDefaultValue(Set.empty)
    val calls = scala.collection.mutable.ListBuffer.empty[(Expr, Id, Int)]
    val sinks = scala.collection.mutable.Set.empty[Id]       // FirstClass usage
    val calleeUses = scala.collection.mutable.Set.empty[Id]   // SecondClass usage (may be promoted)
    val flowEdges = scala.collection.mutable.Set.empty[(Id, Id)]

    flows.foreach {
      case KnownFlow.Data(from, to) =>
        dynamicInputs(to) = dynamicInputs(to) + from
        from match {
          case Expr.Variable(fromId) => flowEdges += ((fromId, to))
          case _ => ()
        }
      case KnownFlow.Call(from, to, index) =>
        calls += ((from, to, index))
      case KnownFlow.Sink(from) =>
        sinks += from
    }

    toplevelParams.foreach { id => sinks += id }

    // The main function's last two parameters are always Toplevel and Return
    knownFunctions.get(main).foreach { sig =>
      val params = sig.params
      if (params.size >= 2) {
        val ks = params(params.size - 2)
        val k = params(params.size - 1)
        dynamicInputs(ks) = dynamicInputs(ks) + Expr.Toplevel
        dynamicInputs(k) = dynamicInputs(k) + Expr.Return
      }
    }

    // --- Phase 2: Propagate inputs forward (fixpoint) ---

    val resolvedCalls = scala.collection.mutable.Set.empty[(Expr, Id, Int)]

    def addInput(from: Expr, to: Id): Unit = {
      dynamicInputs(to) = dynamicInputs(to) + from
      from match {
        case Expr.Variable(fromId) => flowEdges += ((fromId, to))
        case _ => ()
      }
    }

    var changed = true
    while (changed) {
      changed = false

      // Substitute through single-input nodes
      for (id <- dynamicInputs.keys.toList) {
        val oldInputs = dynamicInputs(id)
        var newInputs = Set.empty[Expr]

        oldInputs.foreach {
          case Expr.Variable(other) if other == id =>
            () // drop self-references
          case v @ Expr.Variable(other) =>
            val otherInputs = dynamicInputs(other)
            if (otherInputs.size == 1) newInputs = newInputs ++ otherInputs
            else newInputs = newInputs + v
          case other =>
            newInputs = newInputs + other
        }

        if (newInputs != oldInputs) {
          dynamicInputs(id) = newInputs
          changed = true
        }
      }

      // Resolve calls against known function targets
      for ((from, callee, index) <- calls if !resolvedCalls.contains((from, callee, index))) {
        val calleeInputs = dynamicInputs(callee)

        val targets =
          if (calleeInputs.isEmpty && knownFunctions.contains(callee)) Set(callee)
          else calleeInputs.collect {
            case Expr.Variable(funId) if knownFunctions.contains(funId) => funId
          }

        // Only resolve if ALL callee inputs are known functions
        if (targets.nonEmpty && targets.size >= calleeInputs.size.max(1)) {
          for (funId <- targets) {
            val params = knownFunctions(funId).params
            if (index < params.size) addInput(from, params(index))
          }
          resolvedCalls += ((from, callee, index))
          calleeUses += callee // SecondClass: all targets known
          changed = true
        }
      }
    }

    // Unresolved calls: arguments escape, callee is FirstClass
    for ((from, callee, index) <- calls if !resolvedCalls.contains((from, callee, index))) {
      sinks += callee // FirstClass: not all targets known
      from match {
        case Expr.Variable(id) => sinks += id
        case _ => ()
      }
    }

    // --- Phase 3: Propagate usage backward (fixpoint) ---

    val usage = scala.collection.mutable.Map.empty[Id, Usage].withDefaultValue(Usage.Unused)

    sinks.foreach { id => usage(id) = Usage.FirstClass }
    calleeUses.foreach { id => usage(id) = usage(id).join(Usage.SecondClass) }

    // Build reverse edges
    val reverseEdges = scala.collection.mutable.Map.empty[Id, Set[Id]].withDefaultValue(Set.empty)
    for ((from, to) <- flowEdges) {
      reverseEdges(to) = reverseEdges(to) + from
    }

    changed = true
    while (changed) {
      changed = false
      for ((id, u) <- usage.toList if u != Usage.Unused) {
        for (from <- reverseEdges(id)) {
          val old = usage(from)
          val joined = old.join(u)
          if (joined != old) {
            usage(from) = joined
            changed = true
          }
        }
      }
    }

    // --- Collect results ---

    val allIds = dynamicInputs.keySet ++ usage.keySet
    allIds.map { id =>
      id -> NodeInfo(dynamicInputs(id), usage(id))
    }.toMap
  }

  // We only solve for each toplevel definition individually. This has the following reasons:
  //   1. our JS backend mostly benefits from parameter dropping in local functions / loops, since
  //      more things become second class / do not escape
  //   2. this avoids the problem of functions being mutually recursive (since they are not)
  //
  // Implementation notes:
  // - if something flows into a function that is not part of `functions`, this probably means the
  //   function is a toplevel function or unknown. In this case, the flow should go into ? to say "it is used"
  //     x <: f[0]   where f is unknown (that is, not part of functions)
  //       ~>
  //     x <: ?
  //
  // - if an expression flows into ?,  then all of its free variables flow into ?:
  //     Cons(x, Cons(y, Cons(1, Nil()))) <: ?
  //       ~>
  //     x <: ?, y <: ?
  //
  // - constant flows into ? are discarded
  //     1 <: ?   ~> .
  //
  // - if something flows into a parameter of a known functions, it flows into the parameter
  //     x <: f[0]  where f(a, b, c) { ... } in functions
  //       ~>
  //     x <: a
  //   For this, probably we need some information about all "parameters", which we can gather from `functions`.
  //
  // - higher-order flows: if f <: g, where g is a parameter, and x <: g[0], then x <: g[0], f[0]
  //   this might be tricky to implement efficiently
  //
  // - for main, ks, and k are always used...
  //   ks <: ?, k <: ?
  //
  // - what do we do for:
  //     Cons(f, Nil()) <: x
  //   ?
  //   Probably we just keep it. If x, for example, continues to flow into ? we then mark f as used (f <: ?)
  //


//
//  def solve(toplevel: Id, functions: Map[Id, FunctionFlow]): Unit = {
//    val flows: mutable.HashMap[Id, (FlowSource, FlowTarget)] = mutable.HashMap.empty
//
//    val knownFunctions: Set[Id] = functions.keySet
//    // maps parameters to the function that defines / binds it (if any)
//    val parameters: Map[Id, Id] = functions.flatMap {
//      case (funId, FunctionFlow(_, params, _)) =>
//        params.map(p => p -> funId)
//    }
//
//    var todo: List[Flow] = functions.valuesIterator.flatMap(_.internal).toList
//
//    // direct variable flows (not the transitive closure!)
//    // x <: y
//    var variableFlows: Set[(Id, Id)] = Set.empty
//
//    // if single variables occur here, these are first-class usages of known (!) functions
//    // for example, f <: x where def f() { ... }
//    var inbound: Map[Id, Set[Expr]] = Map.empty
//
//    // x <: f[1]   or   42 <: f[1]
//    var unknownCalls: List[(FlowSource, FlowTarget.Call)] = Nil
//
//    var used: Set[Id] = Set.empty
//
//    var cache: Set[Flow] = Set.empty
//
//    @tailrec
//    def loop(f: Flow => Unit): Unit = todo match {
//      case head :: tail if cache.contains(head) =>
//        todo = tail
//        loop(f)
//      case head :: tail =>
//        cache += head;
//        todo = tail;
//        f(head)
//        loop(f)
//      case Nil => ()
//    }
//
//    def push(others: List[Flow]): Unit =
//      todo = others ::: todo
//
//    loop {
//        // The function x is used in a first-class way (being passed as argument)
//        case Flow(Variable(x), FlowTarget.Variable(y)) if knownFunctions.contains(x) =>
//          inbound += (y -> (inbound.getOrElse(y, Set.empty) + Variable(x)))
//
//        case Flow(Variable(x), FlowTarget.Variable(y)) =>
//          if (x != y) {
//            variableFlows += x -> y
//          }
//          // TODO propagate
//
//        case Flow(exp, FlowTarget.Variable(y)) =>
//          inbound += (y -> (inbound.getOrElse(y, Set.empty) + exp))
//
//        case Flow(Variable(x), FlowTarget.Unknown) =>
//          used += x
//
//        // - if an expression flows into ?,  then all of its free variables flow into ?:
//        //     Cons(x, Cons(y, Cons(1, Nil()))) <: ?
//        //       ~>
//        //     x <: ?, y <: ?
//        // - constant flows into ? are discarded
//        //     1 <: ?   ~> .
//        case Flow(compound, FlowTarget.Unknown) =>
//          push(compound.free.toList.map { x =>
//            Flow(Variable(x), FlowTarget.Unknown)
//          })
//
//        // - if something flows into a parameter of a known functions, it flows into the parameter
//        //     x <: f[0]  where f(a, b, c) { ... } in functions
//        //       ~>
//        //     x <: a
//        case Flow(exp, FlowTarget.Call(fun, index)) if knownFunctions.contains(fun) =>
//          functions(fun) match {
//            case FunctionFlow(_, params, _) =>
//              push(List(Flow(exp, FlowTarget.Variable(params(index)))))
//          }
//
//        // unknown call
//        case Flow(exp, FlowTarget.Call(fun, index)) if parameters.contains(fun) =>
//          unknownCalls = (exp, FlowTarget.Call(fun, index)) :: unknownCalls
//
//          // - higher-order flows: if f <: g, where g is a parameter, and x <: g[0], then x <: g[0], f[0]
//          //   this might be tricky to implement efficiently
//          variableFlows.collect {
//            case (from, to) if to == fun =>
//              // Note: this floods it, which might be too early
//              push(Flow(exp, FlowTarget.Call(from, index)) :: Nil)
//          }
//
//        // not part of our flow
//        case Flow(exp, FlowTarget.Call(fun, index)) =>
//          push(exp.free.toList.map { x =>
//            Flow(Variable(x), FlowTarget.Unknown)
//          })
//
//
//
//        case other => ???
//      }
//
//    // forwarding nodes where there is exactly variable in and one out can be dropped:
//    //
//    //  3            ...          3     ...
//    //    \           /            \      /
//    //     a -> b -> c         ~>   a -> c
//    //    /           \            /      \
//    //  ...           ?          ...       ?
//
//    // for toplevel functions we might not be able to simply drop the parameters since the
//    // callsites in other toplevel functions need to be adapted. We _could_ generate a wrapper
//    // function though (which effectively performs the worker wrapper transformation).
//
//
//    // for each parameter, which is NOT of the toplevel function:
//    // - check whether it is a forwarding node (only has at most one in an at most one out)
//
//    def bindsValues(id: Id): Boolean = inbound.getOrElse(id, Set.empty).nonEmpty
//    def incomingValues(id: Id): Int = inbound.getOrElse(id, Set.empty).size
//    def escapes(id: Id): Boolean = used.contains(id)
//
//    def incomingFlows(id: Id): Int = variableFlows.count(_._2 == id)
//    def usedAsFunction(id: Id): Int = unknownCalls.count(_._2.fun == id)
//
//    def outgoingFlows(id: Id): Int = variableFlows.count(_._1 == id)
//
//
//    // TODO we should also do this for let bindings etc. not only for parameters
//    var forwarding: Set[Id] = Set.empty
//
//    parameters.foreach {
//      case (param, fun) => //if fun != toplevel =>
//        val v = bindsValues(param)
//        val u = escapes(param)
//        val f = usedAsFunction(param)
//        val o = outgoingFlows(param)
//        val i = incomingFlows(param)
//
//        // TODO
//        //  - it is ok if it binds a value, we just need to let bind this value instead of passing it as a parameter.
//        //  - symmetrically, it is ok if it escapes, we "just" need to eta-expand it (is this true??)
//
//        // escapes are ONLY relevant if known function values flow into the parameter, since then the
//        // signature changes. For all other parameters it is ok.
//
//        if (/*!bindsValues(param) && */!escapes(param)) {
//          val indegree = incomingValues(param) + incomingFlows(param)
//          val outdegree = usedAsFunction(param) + outgoingFlows(param)
//
//
//          if indegree <= 1 && outdegree <= 1 then {
//            forwarding += param
//          }
//        }
//    }
//
//    println(s"Parameters in ${toplevel} to be dropped: ${forwarding.mkString(", ")}")
//  }


}



// tmp2 --> x_2 --> ?
// looks like x_2 is a forwarding node, but tmp2 is part of go
// however, x is toplevel and we do not drop toplevel parameters.
