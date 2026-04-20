package effekt
package cpsds

import core.{ Id, ValueType }
import effekt.source.FeatureFlag
import effekt.util.messages.ErrorReporter
import effekt.util.messages.INTERNAL_ERROR


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
}

enum ToplevelDefinition {
  case Def(id: Id, params: List[Id], body: Stmt)
  case Val(id: Id, ks: Id, k: Id, binding: Stmt)

  lazy val escapes: Set[Id] = escapeAnalysis.escapes(this)
  lazy val free: Set[Id] = freeVariables.free(this)
  lazy val uses: DB[Set[Id]] = functionUsage.uses(this)
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
}
export Stmt.*

case class Clause(params: List[Id], body: Stmt) extends Tree {
  lazy val free: Set[Id] = body.free -- params
  lazy val uses: DB[Set[Id]] = functionUsage.uses(this)
  lazy val escapes: Set[Id] = escapeAnalysis.escapes(this)
}

case class Operation(name: Id, params: List[Id], body: Stmt) extends Tree {
  lazy val free: Set[Id] = body.free -- params
  lazy val uses: DB[Set[Id]] = functionUsage.uses(this)
  lazy val escapes: Set[Id] = escapeAnalysis.escapes(this)
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
  // since every function should be only added ONCE we can simply concatenate the DBs
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

/**
 * How often is something (a function or expression) referenced in a subterm?
 */
object references {

  // TODO this might be way to inefficient
  private inline def all[T](terms: Iterable[T], inline run: T => DB[Int]): DB[Int] =
    terms.foldLeft(DB.empty[Int]) { (acc, t) =>
      acc.unionWith(run(t), _ + _)
    }

  inline def refs(expr: Expr): DB[Int] = expr match {
    case Expr.Variable(id) => DB(id,  1)
    case Expr.Literal(value, tpe) => DB.empty
    case Expr.Make(data, tag, args) => all(args, _.refs)
    case Expr.Abort => DB.empty
    case Expr.Return => DB.empty
    case Expr.Toplevel => DB.empty
  }

  inline def refs(stmt: Stmt): DB[Int] = stmt match {
    case Stmt.Def(id, params, body, rest) => ???
    case Stmt.New(id, interface, operations, rest) => ???
    case Stmt.Let(id, binding, rest) => ???
    case Stmt.App(id, args, canBeDirect) => ???
    case Stmt.Invoke(id, method, args) => ???
    case Stmt.Run(id, callee, args, purity, rest) => ???
    case Stmt.If(cond, thn, els) => ???
    case Stmt.Match(scrutinee, clauses, default) => ???
    case Stmt.Region(id, ks, rest) => ???
    case Stmt.Alloc(id, init, region, rest) => ???
    case Stmt.Var(id, init, ks, rest) => ???
    case Stmt.Dealloc(ref, rest) => ???
    case Stmt.Get(ref, id, rest) => ???
    case Stmt.Put(ref, value, rest) => ???
    case Stmt.Reset(p, ks, k, body, ks1, k1) => ???
    case Stmt.Shift(prompt, resume, ks, k, body, ks1, k1) => ???
    case Stmt.Resume(resumption, ks, k, body, ks1, k1) => ???
    case Stmt.Hole(span) => DB.empty
  }
}

//   def f(x, h) {
//     g(x, 1)
//     g(x, 4)
//     h(3)
//   }
// results in
//   g -> ({x}, {1, 4})
//   h -> ({3})
object functionCalls {

}

//   def f(x, y) {
//     g(x, 1)
//     g(x, 4)
//   }
// results in
//   f -> Def(params, body)
// could be extended to values as well for other analyses
object definitions {

}
