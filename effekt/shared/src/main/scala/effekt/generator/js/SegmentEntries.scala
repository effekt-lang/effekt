package effekt
package generator
package js

import effekt.core.Id
import effekt.cps

import java.util.IdentityHashMap
import scala.collection.mutable

/**
 * Tracks continuations that may enter a different delimited-continuation
 * segment.
 *
 * Such an entry can remain raw while it flows through aliases and parameters
 * of a closed, statically known call. Its formal parameter then carries the
 * same property. At an open call or another ordinary value boundary it must be
 * exposed through a stack-safe adapter.
 *
 * The analysis is reachability in a finite graph. Shift and resume bind the
 * roots; aliases and known actual-to-formal transfers are its edges.
 */
object SegmentEntries {

  final class Plan private[SegmentEntries] (
    val entries: Set[Id],
    private val preservingCalls: IdentityHashMap[cps.Stmt.App, java.lang.Boolean]
  ) {
    def contains(id: Id): Boolean = entries.contains(id)

    /** All possible callees are internal definitions with matching arity, so
     *  their formal parameters can retain segment-entry provenance. */
    def preserves(call: cps.Stmt.App): Boolean =
      java.lang.Boolean.TRUE == preservingCalls.get(call)
  }

  def analyze(
    module: cps.ModuleDecl,
    targetFlows: Vector[cps.GuardedEquality.TargetResult]
  ): Plan = {
    require(module.definitions.size == targetFlows.size)

    val parameters = mutable.LinkedHashMap.empty[Id, Vector[Id]]
    module.definitions.foreach {
      case cps.ToplevelDefinition.Def(id, params, _) => parameters(id) = params.toVector
      case _: cps.ToplevelDefinition.Val => ()
    }
    targetFlows.foreach(_.localDefinitions.foreach { definition =>
      parameters(definition.id) = definition.params
    })

    val targetsByCall = new IdentityHashMap[cps.Stmt.App, cps.GuardedEquality.CallTargets]()
    targetFlows.foreach(_.callTargets.foreach { targets =>
      targets.call match {
        case call: cps.Stmt.App => targetsByCall.put(call, targets)
        case _: cps.Stmt.Call => ()
        case _ => ()
      }
    })

    val preservingCalls = new IdentityHashMap[cps.Stmt.App, java.lang.Boolean]()
    val roots = mutable.LinkedHashSet.empty[Id]
    val edges = mutable.LinkedHashMap.empty[Id, mutable.LinkedHashSet[Id]]

    def edge(source: Id, target: Id): Unit =
      edges.getOrElseUpdate(source, mutable.LinkedHashSet.empty) += target

    def closedTargets(call: cps.Stmt.App): Vector[Vector[Id]] =
      parameters.get(call.id) match {
        case Some(params) if params.size == call.args.size => Vector(params)
        case _ =>
          Option(targetsByCall.get(call)).filter(_.closed).fold(Vector.empty) { flow =>
            val targets = flow.targets.toVector
              .sortBy(id => (id.name.name, id.id))
              .flatMap(parameters.get)
            if targets.nonEmpty && targets.forall(_.size == call.args.size) then targets
            else Vector.empty
          }
      }

    def visit(stmt: cps.Stmt): Unit = stmt match {
      case cps.Stmt.Def(_, _, body, rest) => visit(body); visit(rest)
      case cps.Stmt.New(_, _, operations, rest) =>
        operations.foreach(operation => visit(operation.body))
        visit(rest)

      case cps.Stmt.Let(id, cps.Expr.Variable(source), rest) =>
        edge(source, id)
        visit(rest)
      case cps.Stmt.Let(_, _, rest) => visit(rest)

      case cps.Stmt.Call(_, _, _, _, _, rest) => visit(rest)

      case call: cps.Stmt.App =>
        val targets = closedTargets(call)
        if targets.nonEmpty then {
          preservingCalls.put(call, java.lang.Boolean.TRUE)
          call.args.zipWithIndex.foreach {
            case (cps.Expr.Variable(source), index) =>
              targets.foreach(params => edge(source, params(index)))
            case _ => ()
          }
        }

      case _: cps.Stmt.Invoke => ()
      case _: cps.Stmt.Return => ()
      case cps.Stmt.Run(_, _, _, _, rest) => visit(rest)
      case cps.Stmt.If(_, thn, els) => visit(thn); visit(els)
      case cps.Stmt.Match(_, clauses, default) =>
        clauses.foreach { case (_, clause) => visit(clause.body) }
        default.foreach(visit)
      case cps.Stmt.Region(_, _, rest) => visit(rest)
      case cps.Stmt.Alloc(_, _, _, rest) => visit(rest)
      case cps.Stmt.Var(_, _, _, rest) => visit(rest)
      case cps.Stmt.Dealloc(_, rest) => visit(rest)
      case cps.Stmt.Get(_, _, rest) => visit(rest)
      case cps.Stmt.Put(_, _, rest) => visit(rest)
      case cps.Stmt.Reset(_, _, _, body, _, _) => visit(body)
      case cps.Stmt.Shift(_, _, _, k, body, _, _) =>
        roots += k
        visit(body)
      case cps.Stmt.Resume(_, _, k, body, _, _) =>
        roots += k
        visit(body)
      case _: cps.Stmt.Hole => ()
    }

    module.definitions.foreach {
      case cps.ToplevelDefinition.Def(_, _, body) => visit(body)
      case cps.ToplevelDefinition.Val(_, _, _, binding) => visit(binding)
    }

    val entries = mutable.LinkedHashSet.from(roots)
    val pending = mutable.Queue.from(roots)
    while pending.nonEmpty do {
      val source = pending.dequeue()
      edges.get(source).foreach(_.foreach { target =>
        if entries.add(target) then pending.enqueue(target)
      })
    }

    new Plan(entries.toSet, preservingCalls)
  }
}
