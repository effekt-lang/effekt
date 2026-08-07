package effekt
package cpsds

import core.Id

import scala.collection.mutable

object BlockSinking {

  case class Def(id: Id, params: List[Id], body: Stmt)

  def transform(m: ModuleDecl, main: Id): ModuleDecl = {
    val uses = m.uses.toMap

    val toplevelDefs = m.definitions.collect {
      case ToplevelDefinition.Def(id, params, body) => Def(id, params, body)
    }
    val toplevelIds = toplevelDefs.map(_.id).toSet

    // A mutually recursive group cannot become local because local functions
    // in Effekt are not mutually recursive. Self-recursive definitions are
    // fine: their own name is in scope in their body.
    val mutuallyRecursive = toplevelIds.filter { id =>
      val reachable = uses.getOrElse(id, Set.empty) & toplevelIds
      reachable.exists { other =>
        other != id && uses.getOrElse(other, Set.empty).contains(id)
      }
    }

    val anchors = Set(main) ++ mutuallyRecursive
    val candidates = toplevelIds -- anchors

    // A definition can be localized when precisely one anchor transitively
    // uses it. Definitions shared by anchors remain at module scope.
    val ownerOf = candidates.flatMap { candidate =>
      val owners = anchors.filter { anchor =>
        uses.getOrElse(anchor, Set.empty).contains(candidate)
      }
      Option.when(owners.size == 1)(candidate -> owners.head)
    }.toMap

    val sinkable = ownerOf.keySet
    val ownedBy = toplevelDefs
      .filter(d => sinkable.contains(d.id))
      .groupBy(d => ownerOf(d.id))

    val definitions = m.definitions.flatMap {
      case ToplevelDefinition.Def(id, _, _) if sinkable.contains(id) =>
        None

      case ToplevelDefinition.Def(id, params, body) =>
        val localized = localize(ownedBy.getOrElse(id, Nil), body)
        Some(ToplevelDefinition.Def(id, params, localized))

      case ToplevelDefinition.Val(id, ks, k, binding) =>
        Some(ToplevelDefinition.Val(id, ks, k, normalize(binding)))
    }

    m.copy(definitions = definitions)
  }

  /**
   * Turn globally visible definitions into a well-scoped local telescope.
   * Dependencies precede their users; normalization then sinks every binding
   * to the least syntactic scope containing all of its uses.
   */
  private def localize(definitions: List[Def], body: Stmt): Stmt = {
    val telescope = dependencyOrder(definitions).foldRight(body) { case (d, rest) =>
      Stmt.Def(d.id, d.params, d.body, rest)
    }
    normalize(telescope)
  }

  /** Dependency-first order for the one-time module-to-local conversion. */
  private def dependencyOrder(definitions: List[Def]): List[Def] = {
    val byId = definitions.map(d => d.id -> d).toMap
    val ids = byId.keySet
    val position = definitions.zipWithIndex.map { case (d, index) => d.id -> index }.toMap
    val visited = mutable.Set.empty[Id]
    val result = mutable.ListBuffer.empty[Def]

    def visit(d: Def): Unit = {
      if (!visited.add(d.id)) return

      val dependencies = (d.body.free & ids).toList.sortBy(position.apply)
      dependencies.foreach(id => visit(byId(id)))
      result += d
    }

    definitions.foreach(visit)
    result.toList
  }

  /** Normalize all existing local definitions from the inside out. */
  private def normalize(stmt: Stmt): Stmt = rewriting(stmt) {
    case Stmt.Def(id, params, body, rest) =>
      sink(Def(id, params, normalize(body)), normalize(rest))

    case Stmt.New(id, interface, operations, rest) =>
      val operations1 = operations.map { op =>
        Operation(op.name, op.params, normalize(op.body))
      }
      Stmt.New(id, interface, operations1, normalize(rest))

    case Stmt.Let(id, binding, rest) =>
      Stmt.Let(id, binding, normalize(rest))

    case Stmt.App(_, _, _) | Stmt.Invoke(_, _, _) | Stmt.Hole(_) =>
      stmt

    case Stmt.Run(id, callee, args, purity, rest) =>
      Stmt.Run(id, callee, args, purity, normalize(rest))

    case Stmt.If(cond, thn, els) =>
      Stmt.If(cond, normalize(thn), normalize(els))

    case Stmt.Match(scrutinee, clauses, default) =>
      val clauses1 = clauses.map { case (tag, clause) =>
        tag -> Clause(clause.params, normalize(clause.body))
      }
      Stmt.Match(scrutinee, clauses1, default.map(normalize))

    case Stmt.Region(id, ks, rest) =>
      Stmt.Region(id, ks, normalize(rest))

    case Stmt.Alloc(id, init, region, rest) =>
      Stmt.Alloc(id, init, region, normalize(rest))

    case Stmt.Var(id, init, ks, rest) =>
      Stmt.Var(id, init, ks, normalize(rest))

    case Stmt.Dealloc(ref, rest) =>
      Stmt.Dealloc(ref, normalize(rest))

    case Stmt.Get(ref, id, rest) =>
      Stmt.Get(ref, id, normalize(rest))

    case Stmt.Put(ref, value, rest) =>
      Stmt.Put(ref, value, normalize(rest))

    case Stmt.Reset(p, ks, k, body, ks1, k1) =>
      Stmt.Reset(p, ks, k, normalize(body), ks1, k1)

    case Stmt.Shift(prompt, resume, ks, k, body, ks1, k1) =>
      Stmt.Shift(prompt, resume, ks, k, normalize(body), ks1, k1)

    case Stmt.Resume(resumption, ks, k, body, ks1, k1) =>
      Stmt.Resume(resumption, ks, k, normalize(body), ks1, k1)
  }

  /**
   * Repeatedly apply the inward commuting conversion for one definition.
   * A definition moves into a constructor exactly when all of its uses occur
   * in one admissible child region. Otherwise this is its least common scope.
   */
  private def sink(d: Def, stmt: Stmt): Stmt = {
    // Function definitions are pure, so an unused definition can be dropped.
    if (!stmt.free.contains(d.id)) return stmt

    stmt match {
      case Stmt.Def(id, params, body, rest) =>
        (body.free.contains(d.id), rest.free.contains(d.id)) match {
          case (true, false) => Stmt.Def(id, params, sink(d, body), rest)
          case (false, true) => Stmt.Def(id, params, body, sink(d, rest))
          case _ => bind(d, stmt)
        }

      // Operation bodies are barriers for now, matching the previous pass.
      case Stmt.New(id, interface, operations, rest) =>
        if (operations.exists(_.free.contains(d.id))) bind(d, stmt)
        else Stmt.New(id, interface, operations, sink(d, rest))

      case Stmt.Let(id, binding, rest) =>
        if (binding.free.contains(d.id)) bind(d, stmt)
        else Stmt.Let(id, binding, sink(d, rest))

      case Stmt.App(_, _, _) | Stmt.Invoke(_, _, _) =>
        bind(d, stmt)

      case Stmt.Run(id, callee, args, purity, rest) =>
        val usedImmediately = callee == d.id || args.exists(_.free.contains(d.id))
        if (usedImmediately) bind(d, stmt)
        else Stmt.Run(id, callee, args, purity, sink(d, rest))

      case Stmt.If(cond, thn, els) =>
        if (cond.free.contains(d.id)) bind(d, stmt)
        else (thn.free.contains(d.id), els.free.contains(d.id)) match {
          case (true, false) => Stmt.If(cond, sink(d, thn), els)
          case (false, true) => Stmt.If(cond, thn, sink(d, els))
          case _ => bind(d, stmt)
        }

      case Stmt.Match(scrutinee, clauses, default) =>
        if (scrutinee.free.contains(d.id)) bind(d, stmt)
        else {
          val clauseUses = clauses.map { case (_, clause) => clause.free.contains(d.id) }
          val defaultUses = default.exists(_.free.contains(d.id))
          val regionCount = clauseUses.count(identity) + Option.when(defaultUses)(1).getOrElse(0)

          if (regionCount != 1) bind(d, stmt)
          else if (defaultUses) Stmt.Match(scrutinee, clauses, default.map(sink(d, _)))
          else {
            val clauses1 = clauses.zip(clauseUses).map {
              case ((tag, clause), true) =>
                tag -> Clause(clause.params, sink(d, clause.body))
              case ((tag, clause), false) =>
                tag -> clause
            }
            Stmt.Match(scrutinee, clauses1, default)
          }
        }

      case Stmt.Region(id, ks, rest) =>
        if (ks.free.contains(d.id)) bind(d, stmt)
        else Stmt.Region(id, ks, sink(d, rest))

      case Stmt.Alloc(id, init, region, rest) =>
        if (init.free.contains(d.id) || region == d.id) bind(d, stmt)
        else Stmt.Alloc(id, init, region, sink(d, rest))

      case Stmt.Var(id, init, ks, rest) =>
        if (init.free.contains(d.id) || ks.free.contains(d.id)) bind(d, stmt)
        else Stmt.Var(id, init, ks, sink(d, rest))

      case Stmt.Dealloc(ref, rest) =>
        if (ref == d.id) bind(d, stmt)
        else Stmt.Dealloc(ref, sink(d, rest))

      case Stmt.Get(ref, id, rest) =>
        if (ref == d.id) bind(d, stmt)
        else Stmt.Get(ref, id, sink(d, rest))

      case Stmt.Put(ref, value, rest) =>
        if (ref == d.id || value.free.contains(d.id)) bind(d, stmt)
        else Stmt.Put(ref, value, sink(d, rest))

      case Stmt.Reset(p, ks, k, body, ks1, k1) =>
        if (ks1.free.contains(d.id) || k1.free.contains(d.id)) bind(d, stmt)
        else Stmt.Reset(p, ks, k, sink(d, body), ks1, k1)

      case Stmt.Shift(prompt, resume, ks, k, body, ks1, k1) =>
        val usedImmediately =
          prompt == d.id || ks1.free.contains(d.id) || k1.free.contains(d.id)
        if (usedImmediately) bind(d, stmt)
        else Stmt.Shift(prompt, resume, ks, k, sink(d, body), ks1, k1)

      case Stmt.Resume(resumption, ks, k, body, ks1, k1) =>
        val usedImmediately =
          resumption == d.id || ks1.free.contains(d.id) || k1.free.contains(d.id)
        if (usedImmediately) bind(d, stmt)
        else Stmt.Resume(resumption, ks, k, sink(d, body), ks1, k1)

      case Stmt.Hole(_) =>
        bind(d, stmt)
    }
  }

  private def bind(d: Def, rest: Stmt): Stmt =
    Stmt.Def(d.id, d.params, d.body, rest)
}
