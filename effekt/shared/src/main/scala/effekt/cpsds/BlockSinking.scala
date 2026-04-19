package effekt
package cpsds

import core.Id

object BlockSinking {

  def transform(m: ModuleDecl, main: Id): ModuleDecl = {
    given ctx: Context = Context(m.uses)

    val uses = m.uses

    // 1. Compute mutual recursion clusters (SCCs of size > 1)
    val toplevelIds = uses.keySet
    val mutuallyRecursive: Set[Id] = {
      // Simple SCC: f and g are mutually recursive if f ∈ uses(g) and g ∈ uses(f)
      // We need transitive closure, but uses is already transitively closed at toplevel
      toplevelIds.filter { id =>
        val reachable = uses.getOrElse(id, Set.empty) & toplevelIds
        reachable.exists { other =>
          other != id && uses.getOrElse(other, Set.empty).contains(id)
        }
      }
    }

    // 2. Anchors: defs that must stay at toplevel
    //    - mutually recursive defs
    //    - exported defs
    //    - Val definitions (they're not Defs, can't be sunk)
    val exported = m.exports.toSet
    val anchors: Set[Id] = Set(main) ++ mutuallyRecursive // ++ exported

    // 3. For each non-anchor toplevel Def, determine which anchors use it.
    //    "uses" maps definer -> used, so we invert: for each candidate,
    //    which anchors reference it (transitively)?
    val candidates = toplevelIds -- anchors

    val usedByAnchor: Map[Id, Set[Id]] = candidates.map { cand =>
      cand -> anchors.filter { anchor =>
        uses.getOrElse(anchor, Set.empty).contains(cand)
      }
    }.toMap

    // 4. A candidate is sinkable if exactly one anchor uses it
    val sinkable: Set[Id] = usedByAnchor.collect {
      case (id, anchors) if anchors.size == 1 => id
    }.toSet

    // 5. Build pending from sinkable defs
    val pending: Pending = m.definitions.collect {
      case ToplevelDefinition.Def(id, params, body) if sinkable.contains(id) =>
        id -> (params, body)
    }.toMap

    // 6. Transform: keep non-sinkable defs and Vals at toplevel,
    //    each body gets the full pending set and will place what it needs
    val newDefinitions = m.definitions.flatMap {
      case ToplevelDefinition.Def(id, params, body) if sinkable.contains(id) =>
        None // will be sunk
      case ToplevelDefinition.Def(id, params, body) =>
        Some(ToplevelDefinition.Def(id, params, transform(body, pending)))
      case ToplevelDefinition.Val(id, ks, k, binding) =>
        Some(ToplevelDefinition.Val(id, ks, k, transform(binding, pending)))
    }

    m.copy(definitions = newDefinitions)
  }

  case class Context(
    uses: Map[Id, Set[Id]]
  ) {
    def close(ids: Set[Id], pending: Pending): Set[Id] = {
      var result = ids
      var worklist = ids
      while (worklist.nonEmpty) {
        val next = worklist.flatMap { id =>
          uses.getOrElse(id, Set.empty).filter(pending.contains)
        } -- result
        result = result ++ next
        worklist = next
      }
      result
    }
  }

  type Pending = Map[Id, (List[Id], Stmt)]

  extension (pending: Pending) {

    /** Which pending defs are free in the given tree? */
    def usedIn(tree: Stmt): Set[Id] =
      pending.keySet & tree.free

    def usedIn(tree: Expr): Set[Id] =
      pending.keySet & tree.free

    def usedIn(tree: Clause): Set[Id] =
      pending.keySet & tree.free

    def usedIn(tree: Operation): Set[Id] =
      pending.keySet & tree.free

    def usedIn(ids: Set[Id]): Set[Id] =
      pending.keySet & ids

    /** Remove a set of ids from pending */
    def without(ids: Set[Id]): Pending =
      pending -- ids

    /** Only keep the given ids */
    def only(ids: Set[Id]): Pending =
      pending.view.filterKeys(ids).toMap

    /**
     * Partition pending into (emitHere, remaining) given forced ids.
     * Closes forced under dependencies.
     */
    def emit(forced: Set[Id])(using ctx: Context): (Set[Id], Pending) = {
      val emitHere = ctx.close(forced, pending)
      (emitHere, pending.without(emitHere))
    }
  }

  def transform(stmt: Stmt, pending: Pending)(using ctx: Context): Stmt = rewriting(stmt) {

    case Stmt.Def(id, params, body, rest) =>
      transform(rest, pending + (id -> (params, body)))

    case Stmt.If(cond, thn, els) =>
      val inBoth = pending.usedIn(thn) & pending.usedIn(els)
      val (emitHere, remaining) = pending.emit(pending.usedIn(cond) ++ inBoth)

      val thnOnly = ctx.close(remaining.usedIn(thn), remaining) -- emitHere
      val elsOnly = ctx.close(remaining.usedIn(els), remaining) -- emitHere

      val result = Stmt.If(
        cond,
        transform(thn, remaining.only(thnOnly)),
        transform(els, remaining.only(elsOnly))
      )
      emitDefs(emitHere, pending, result)

    case Stmt.Match(scrutinee, clauses, default) =>
      val branchSets: List[Set[Id]] = clauses.map { case (_, cl) => pending.usedIn(cl) } ++
        default.map(pending.usedIn).toList
      val useCounts = branchSets.flatten.groupBy(identity).view.mapValues(_.size)
      val inMultiple = useCounts.collect { case (id, n) if n > 1 => id }.toSet

      val (emitHere, remaining) = pending.emit(pending.usedIn(scrutinee) ++ inMultiple)

      val clauses1 = clauses.map { case (tag, cl) =>
        val branchNeeds = ctx.close(remaining.usedIn(cl), remaining) -- emitHere
        (tag, Clause(cl.params, transform(cl.body, remaining.only(branchNeeds))))
      }
      val default1 = default.map { d =>
        val branchNeeds = ctx.close(remaining.usedIn(d), remaining) -- emitHere
        transform(d, remaining.only(branchNeeds))
      }
      emitDefs(emitHere, pending, Stmt.Match(scrutinee, clauses1, default1))

    case Stmt.Let(id, binding, rest) =>
      val (emitHere, remaining) = pending.emit(pending.usedIn(binding))
      emitDefs(emitHere, pending, Stmt.Let(id, binding, transform(rest, remaining)))

    case Stmt.Run(id, callee, args, purity, rest) =>
      val (emitHere, remaining) = pending.emit(args.foldLeft(pending.usedIn(Set(callee))) {
        (acc, a) => acc ++ pending.usedIn(a)
      })
      emitDefs(emitHere, pending, Stmt.Run(id, callee, args, purity, transform(rest, remaining)))

    case Stmt.New(id, interface, operations, rest) =>
      val (emitHere, remaining) = pending.emit(operations.foldLeft(Set.empty[Id]) {
        (acc, op) => acc | pending.usedIn(op)
      })
      val ops1 = operations.map(op => Operation(op.name, op.params, transform(op.body, Map.empty)))
      emitDefs(emitHere, pending, Stmt.New(id, interface, ops1, transform(rest, remaining)))

    case Stmt.Region(id, ks, rest) =>
      Stmt.Region(id, ks, transform(rest, pending))

    case Stmt.Alloc(id, init, region, rest) =>
      val (emitHere, remaining) = pending.emit(pending.usedIn(init))
      emitDefs(emitHere, pending, Stmt.Alloc(id, init, region, transform(rest, remaining)))

    case Stmt.Var(id, init, ks, rest) =>
      val (emitHere, remaining) = pending.emit(pending.usedIn(init) ++ pending.usedIn(ks))
      emitDefs(emitHere, pending, Stmt.Var(id, init, ks, transform(rest, remaining)))

    case Stmt.Dealloc(ref, rest) =>
      val (emitHere, remaining) = pending.emit(pending.usedIn(Set(ref)))
      emitDefs(emitHere, pending, Stmt.Dealloc(ref, transform(rest, remaining)))

    case Stmt.Get(ref, id, rest) =>
      val (emitHere, remaining) = pending.emit(pending.usedIn(Set(ref)))
      emitDefs(emitHere, pending, Stmt.Get(ref, id, transform(rest, remaining)))

    case Stmt.Put(ref, value, rest) =>
      val (emitHere, remaining) = pending.emit(pending.usedIn(value) ++ pending.usedIn(Set(ref)))
      emitDefs(emitHere, pending, Stmt.Put(ref, value, transform(rest, remaining)))

    case Stmt.Reset(p, ks, k, body, ks1, k1) =>
      val (emitHere, remaining) = pending.emit(pending.usedIn(ks1) ++ pending.usedIn(k1))
      emitDefs(emitHere, pending, Stmt.Reset(p, ks, k, transform(body, remaining), ks1, k1))

    case Stmt.Shift(prompt, resume, ks, k, body, ks1, k1) =>
      val (emitHere, remaining) = pending.emit(pending.usedIn(Set(prompt)) ++ pending.usedIn(ks1) ++ pending.usedIn(k1))
      emitDefs(emitHere, pending, Stmt.Shift(prompt, resume, ks, k, transform(body, remaining), ks1, k1))

    case Stmt.Resume(resumption, ks, k, body, ks1, k1) =>
      val (emitHere, remaining) = pending.emit(pending.usedIn(Set(resumption)) ++ pending.usedIn(ks1) ++ pending.usedIn(k1))
      emitDefs(emitHere, pending, Stmt.Resume(resumption, ks, k, transform(body, remaining), ks1, k1))

    case Stmt.App(_, _, _) | Stmt.Invoke(_, _, _) | Stmt.Hole(_) =>
      val (emitHere, _) = pending.emit(pending.usedIn(stmt))
      emitDefs(emitHere, pending, stmt)
  }

  def emitDefs(ids: Set[Id], pending: Pending, body: Stmt)(using ctx: Context): Stmt = {
    if (ids.isEmpty) return body
    topoSort(ids, pending).foldRight(body) { case (id, rest) =>
      val (params, defBody) = pending(id)
      Stmt.Def(id, params, transform(defBody, Map.empty), rest)
    }
  }

  def topoSort(ids: Set[Id], pending: Pending)(using ctx: Context): List[Id] = {
    var visited = Set.empty[Id]
    var result = List.empty[Id]
    def visit(id: Id): Unit = {
      if (visited.contains(id)) return
      visited += id
      (ctx.uses.getOrElse(id, Set.empty) & ids).foreach(visit)
      result = result :+ id
    }
    ids.foreach(visit)
    result
  }
}
