package effekt
package cpsds

import core.Id

object BlockSinking {

  def transform(m: ModuleDecl, main: Id): ModuleDecl = {
    given ctx: Context = Context(m.uses)

    val uses = m.uses.debug

    // 1. Compute mutual recursion clusters (SCCs of size > 1)
    val toplevelIds = m.definitions.collect {
      case ToplevelDefinition.Def(id, _, _) => id
    }.toSet

    val mutuallyRecursive: Set[Id] = {
      // Simple SCC: f and g are mutually recursive if f ∈ uses(g) and g ∈ uses(f)
      // We need transitive closure, but uses is already transitively closed at toplevel
      toplevelIds.filter { id =>
        val reachable = uses.getOrElse(id, Set.empty[Id]) & toplevelIds
        reachable.exists { other =>
          other != id && uses.getOrElse(other, Set.empty).contains(id)
        }
      }
    }

    // 2. Anchors: defs that must stay at toplevel
    //    - mutually recursive defs
    //    - exported defs
    //    - Val definitions (they're not Defs, can't be sunk)
    val exported: Set[Id] = m.exports.toSet
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
        id -> Def(id, params, body)
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

    val result = m.copy(definitions = newDefinitions)

    result
  }

  case class Context(
    uses: DB[Set[Id]]
  ) {
    def close(ids: Set[Id], pending: Pending): Set[Id] = {
      var result = ids
      var worklist = ids
      while (worklist.nonEmpty) {
        val next = worklist.flatMap { id =>
          uses.getOrElse(id, Set.empty[Id]).filter(pending.contains)
        } -- result
        result = result ++ next
        worklist = next
      }
      result
    }
  }
  case class Def(id: Id, params: List[Id], body: Stmt)
  type Pending = Map[Id, Def]

  extension (pending: Pending) {

    /** Which pending defs are free in the given tree? */
    def usedIn(tree: Stmt | Expr | Clause | Operation): Set[Id] =
      pending.keySet & (tree match {
        case t: Stmt => t.free
        case e: Expr => e.free
        case c: Clause => c.free
        case o: Operation => o.free
      })

    def usedIn(ids: Set[Id]): Set[Id] =
      pending.keySet & ids

    /**
     * Emit forced defs (transitively closed) before the body.
     * The callback receives the remaining pending defs.
     */
    def emit(forced: Set[Id])(body: Pending => Stmt)(using ctx: Context): Stmt = {
      val emitHere = ctx.close(forced, pending)
      val remaining = pending -- emitHere
      emitDefs(emitHere, pending, body(remaining))
    }

    def forBranch(tree: Stmt | Clause)(using ctx: Context): Pending =
      val transitiveClosure = ctx.close(pending.usedIn(tree), pending)
      pending.view.filterKeys(transitiveClosure).toMap
  }

  def transform(stmt: Stmt, pending: Pending)(using ctx: Context): Stmt = rewriting(stmt) {

    case Stmt.Def(id, params, body, rest) =>
      transform(rest, pending + (id -> Def(id, params, body)))

    case Stmt.If(cond, thn, els) =>
      val inBoth = pending.usedIn(thn) & pending.usedIn(els)
      pending.emit(pending.usedIn(cond) ++ inBoth) { remaining =>
        Stmt.If(
          cond,
          transform(thn, remaining.forBranch(thn)),
          transform(els, remaining.forBranch(els))
        )
      }

    case Stmt.Match(scrutinee, clauses, default) =>
      val branchSets: List[Set[Id]] = clauses.map { case (_, cl) => pending.usedIn(cl) } ++
        default.map(pending.usedIn).toList
      val useCounts = branchSets.flatten.groupBy(identity).view.mapValues(_.size)
      val inMultiple = useCounts.collect { case (id, n) if n > 1 => id }.toSet

      pending.emit(pending.usedIn(scrutinee) ++ inMultiple) { remaining =>
        val clauses1 = clauses.map { case (tag, cl) =>
          (tag, Clause(cl.params, transform(cl.body, remaining.forBranch(cl))))
        }
        val default1 = default.map { d => transform(d, remaining.forBranch(d)) }
        Stmt.Match(scrutinee, clauses1, default1)
      }

    case Stmt.Let(id, binding, rest) =>
      pending.emit(pending.usedIn(binding)) { remaining =>
        Stmt.Let(id, binding, transform(rest, remaining))
      }

    case Stmt.Run(id, callee, args, purity, rest) =>
      pending.emit(args.foldLeft(pending.usedIn(Set(callee))) {
        (acc, a) => acc ++ pending.usedIn(a)
      }) { remaining =>
        Stmt.Run(id, callee, args, purity, transform(rest, remaining))
      }

    case Stmt.New(id, interface, operations, rest) =>
      pending.emit(operations.foldLeft(Set.empty[Id]) {
        (acc, op) => acc | pending.usedIn(op)
      }) { remaining =>
        val ops1 = operations.map(op => Operation(op.name, op.params, transform(op.body, Map.empty)))
        Stmt.New(id, interface, ops1, transform(rest, remaining))
      }

    case Stmt.Region(id, ks, rest) =>
      Stmt.Region(id, ks, transform(rest, pending))

    case Stmt.Alloc(id, init, region, rest) =>
      pending.emit(pending.usedIn(init)) { remaining =>
        Stmt.Alloc(id, init, region, transform(rest, remaining))
      }

    case Stmt.Var(id, init, ks, rest) =>
      pending.emit(pending.usedIn(init) ++ pending.usedIn(ks)) { remaining =>
        Stmt.Var(id, init, ks, transform(rest, remaining))
      }

    case Stmt.Dealloc(ref, rest) =>
      pending.emit(pending.usedIn(Set(ref))) { remaining =>
        Stmt.Dealloc(ref, transform(rest, remaining))
      }

    case Stmt.Get(ref, id, rest) =>
      pending.emit(pending.usedIn(Set(ref))) { remaining =>
        Stmt.Get(ref, id, transform(rest, remaining))
      }

    case Stmt.Put(ref, value, rest) =>
      pending.emit(pending.usedIn(value) ++ pending.usedIn(Set(ref))) { remaining =>
        Stmt.Put(ref, value, transform(rest, remaining))
      }

    case Stmt.Reset(p, ks, k, body, ks1, k1) =>
      pending.emit(pending.usedIn(ks1) ++ pending.usedIn(k1)) { remaining =>
        Stmt.Reset(p, ks, k, transform(body, remaining), ks1, k1)
      }

    case Stmt.Shift(prompt, resume, ks, k, body, ks1, k1) =>
      pending.emit(pending.usedIn(Set(prompt)) ++ pending.usedIn(ks1) ++ pending.usedIn(k1)) { remaining =>
        Stmt.Shift(prompt, resume, ks, k, transform(body, remaining), ks1, k1)
      }

    case Stmt.Resume(resumption, ks, k, body, ks1, k1) =>
      pending.emit(pending.usedIn(Set(resumption)) ++ pending.usedIn(ks1) ++ pending.usedIn(k1)) { remaining =>
        Stmt.Resume(resumption, ks, k, transform(body, remaining), ks1, k1)
      }

    case Stmt.App(_, _, _) | Stmt.Invoke(_, _, _) | Stmt.Hole(_) =>
      pending.emit(pending.usedIn(stmt)) { _ => stmt }
  }

  private def emitDefs(ids: Set[Id], pending: Pending, body: Stmt)(using ctx: Context): Stmt = {
    if (ids.isEmpty) return body
    topoSort(ids, pending).foldRight(body) { case (id, rest) =>
      val Def(_, params, defBody) = pending(id)
      Stmt.Def(id, params, transform(defBody, Map.empty), rest)
    }
  }

  private def topoSort(ids: Set[Id], pending: Pending)(using ctx: Context): List[Id] = {
    var visited = Set.empty[Id]
    var result = List.empty[Id]
    def visit(id: Id): Unit = {
      if (visited.contains(id)) return
      visited += id
      (ctx.uses.getOrElse(id, Set.empty[Id]) & ids).foreach(visit)
      result = result :+ id
    }
    ids.foreach(visit)
    result
  }
}
