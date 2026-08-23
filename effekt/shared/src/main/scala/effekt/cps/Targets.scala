package effekt
package cps

import core.Id
import scala.collection.mutable

/** Call targets obtained from the collecting projection of [[AbstractMachine]].
 *
 * Each top-level definition is analysed in isolation; references to sibling
 * top-levels are therefore free and opaque.
 */
object Targets {

  final case class LocalDefinition(
    id: Id,
    params: Vector[Id],
    body: Stmt,
    captures: Vector[Id]
  )

  final case class CallTargets(
    call: Stmt,
    callee: Id,
    arity: Int,
    targets: Set[Id],
    closed: Boolean
  )

  final case class TargetResult(
    localDefinitions: Vector[LocalDefinition],
    callTargets: Vector[CallTargets],
    rigidFunctions: Set[Id],
    escapedFunctions: Set[Id]
  ) {
    def isRigid(function: Id): Boolean = rigidFunctions.contains(function)
    def escapes(function: Id): Boolean = escapedFunctions.contains(function)
  }

  def targets(toplevel: ToplevelDefinition): TargetResult = {
    val single = ModuleDecl(Nil, Nil, Nil, List(toplevel), Nil)
    val pointsTo = new PointsTo(single)

    val (params, body) = toplevel match {
      case ToplevelDefinition.Def(_, ps, b) => (ps.toVector, b)
      case ToplevelDefinition.Val(_, ks, k, b) => (Vector(ks, k), b)
    }

    val locals = collectLocalDefinitions(body, params)
    val callTargets = collectSites(body).map { case (statement, callee, arity) =>
      CallTargets(statement, callee, arity, pointsTo.targetsAt(statement), pointsTo.closedAt(statement))
    }

    TargetResult(locals, callTargets, pointsTo.rigidFunctions, pointsTo.escapedFunctions)
  }

  /** Nested definitions with their captured (free, lexically enclosed) variables. */
  private def collectLocalDefinitions(body: Stmt, params: Vector[Id]): Vector[LocalDefinition] = {
    val locals = mutable.ListBuffer.empty[LocalDefinition]

    def go(stmt: Stmt, scope: Vector[Id]): Unit = stmt match {
      case Stmt.Def(id, ps, functionBody, rest) =>
        val captures = scope.filter(functionBody.free.contains)
        locals += LocalDefinition(id, ps.toVector, functionBody, captures)
        go(functionBody, scope ++ Vector(id) ++ ps)
        go(rest, scope :+ id)
      case Stmt.New(id, _, operations, rest) =>
        operations.foreach(operation => go(operation.body, scope ++ operation.params))
        go(rest, scope :+ id)
      case Stmt.Let(id, _, rest) => go(rest, scope :+ id)
      case Stmt.Call(ids, returnedKs, _, _, _, rest) => go(rest, scope ++ ids :+ returnedKs)
      case Stmt.Run(id, _, _, _, rest) => go(rest, scope :+ id)
      case Stmt.If(_, thn, els) => go(thn, scope); go(els, scope)
      case Stmt.Match(_, clauses, default) =>
        clauses.foreach { case (_, clause) => go(clause.body, scope ++ clause.params) }
        default.foreach(go(_, scope))
      case Stmt.Region(id, _, rest) => go(rest, scope :+ id)
      case Stmt.Alloc(id, _, _, rest) => go(rest, scope :+ id)
      case Stmt.Var(id, _, _, rest) => go(rest, scope :+ id)
      case Stmt.Dealloc(_, rest) => go(rest, scope)
      case Stmt.Get(_, id, rest) => go(rest, scope :+ id)
      case Stmt.Put(_, _, rest) => go(rest, scope)
      case Stmt.Reset(p, ks, k, resetBody, _, _) => go(resetBody, scope ++ Vector(p, ks, k))
      case Stmt.Shift(_, resume, ks, k, shiftBody, _, _) => go(shiftBody, scope ++ Vector(resume, ks, k))
      case Stmt.Resume(_, ks, k, resumeBody, _, _) => go(resumeBody, scope ++ Vector(ks, k))
      case _: Stmt.App | _: Stmt.Invoke | _: Stmt.Return | _: Stmt.Hole => ()
    }

    go(body, params)
    locals.toVector
  }

  /** Function-application call sites: `App` and control-pure `Call` to a known
   *  function. A `Call` contributes its `ks` and `k` parameters to the arity. */
  private def collectSites(body: Stmt): Vector[(Stmt, Id, Int)] = {
    val sites = mutable.ListBuffer.empty[(Stmt, Id, Int)]

    def go(stmt: Stmt): Unit = stmt match {
      case Stmt.Def(_, _, functionBody, rest) => go(functionBody); go(rest)
      case Stmt.New(_, _, operations, rest) => operations.foreach(operation => go(operation.body)); go(rest)
      case Stmt.Let(_, _, rest) => go(rest)
      case call @ Stmt.Call(_, _, Callee.Function(id), args, _, rest) =>
        sites += ((call, id, args.size + 2)); go(rest)
      case Stmt.Call(_, _, _, _, _, rest) => go(rest)
      case app @ Stmt.App(id, args) => sites += ((app, id, args.size))
      case Stmt.Run(_, _, _, _, rest) => go(rest)
      case Stmt.If(_, thn, els) => go(thn); go(els)
      case Stmt.Match(_, clauses, default) =>
        clauses.foreach { case (_, clause) => go(clause.body) }; default.foreach(go)
      case Stmt.Region(_, _, rest) => go(rest)
      case Stmt.Alloc(_, _, _, rest) => go(rest)
      case Stmt.Var(_, _, _, rest) => go(rest)
      case Stmt.Dealloc(_, rest) => go(rest)
      case Stmt.Get(_, _, rest) => go(rest)
      case Stmt.Put(_, _, rest) => go(rest)
      case Stmt.Reset(_, _, _, resetBody, _, _) => go(resetBody)
      case Stmt.Shift(_, _, _, _, shiftBody, _, _) => go(shiftBody)
      case Stmt.Resume(_, _, _, resumeBody, _, _) => go(resumeBody)
      case _: Stmt.Invoke | _: Stmt.Return | _: Stmt.Hole => ()
    }

    go(body)
    sites.toVector
  }
}
