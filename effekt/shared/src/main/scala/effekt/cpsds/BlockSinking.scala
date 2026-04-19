package effekt
package cpsds

import core.Id
import scala.collection.mutable

object BlockSinking {

  /**
   * Sink toplevel Def definitions into the functions that dominate them
   * in the call graph, restoring block structure.
   *
   * A function g dominated by f in the call graph can be moved into f's body
   * as a local Stmt.Def. Self-recursive functions can be sunk (the enclosing
   * Stmt.Def scopes over the body). Mutually recursive siblings (same parent
   * in the dominator tree) cannot be sunk since our IR has no mutual letrec.
   *
   * Children of the virtual root are never sunk — they stay at toplevel.
   */
  def transform(entrypoint: Id, m: ModuleDecl): ModuleDecl = {

    // Separate Defs from everything else (Vals stay untouched)
    val (defs, rest) = m.definitions.partition {
      case _: ToplevelDefinition.Def => true
      case _ => false
    }

    val defMap: Map[Id, ToplevelDefinition.Def] = defs.collect {
      case d: ToplevelDefinition.Def => d.id -> d
    }.toMap

    val defIds: Set[Id] = defMap.keySet

    // --- Step 1: Build call graph ---

    // Edge from f to g means f's body references g (both are toplevel defs).
    // Self-edges are excluded since they don't affect dominance.
    val callSuccs: Map[Id, Set[Id]] = defMap.map { case (id, d) =>
      val bodyFree = d.body.free -- d.params.toSet
      id -> (bodyFree.intersect(defIds) - id)
    }

    // Virtual root connects to entrypoint, exports, and orphan defs
    val virtualRoot = Id("__root__")
    val allReferenced = callSuccs.values.flatten.toSet
    val rootTargets = Set(entrypoint) ++ (defIds -- allReferenced)

    val succs: Map[Id, Set[Id]] = callSuccs + (virtualRoot -> rootTargets)
    val allNodes: Set[Id] = defIds + virtualRoot

    // --- Step 2: Compute dominator tree ---

    val preds: Map[Id, Set[Id]] = {
      val p = mutable.Map.empty[Id, Set[Id]]
      for ((from, tos) <- succs; to <- tos)
        p(to) = p.getOrElse(to, Set.empty) + from
      p.toMap
    }

    val rpo = reversePostorder(virtualRoot, succs, allNodes)
    val idom = computeDominators(allNodes.toList, virtualRoot, preds, rpo)

    // dominator tree: parent → list of children
    val domChildren: Map[Id, List[Id]] = {
      val dc = mutable.Map.empty[Id, mutable.ListBuffer[Id]]
      for ((node, parent) <- idom if node != virtualRoot && parent != node)
        dc.getOrElseUpdate(parent, mutable.ListBuffer.empty) += node
      dc.map { case (k, v) => k -> v.toList }.toMap
    }

    // --- Step 3: Determine which defs to sink ---

    // Children of virtualRoot stay at toplevel. Only their descendants can be sunk.
    val toplevelIds: Set[Id] = domChildren.getOrElse(virtualRoot, Nil).toSet

    /**
     * For a given parent, determine which of its dominator-tree children
     * can be sunk into it and which must remain at their current level.
     *
     * Mutually recursive siblings (SCC of size > 1 among siblings) cannot
     * be sunk since our IR doesn't support mutual letrec locally.
     */
    def sinkableChildren(parentId: Id): (List[Id], List[Id]) = {
      val children = domChildren.getOrElse(parentId, Nil)
      if (children.size <= 1) return (children, Nil)

      // Build subgraph among siblings
      val siblingSet = children.toSet
      val siblingSuccs = children.map { c =>
        c -> callSuccs.getOrElse(c, Set.empty).intersect(siblingSet)
      }.toMap

      val sccs = tarjanSCC(children, siblingSuccs)
      val (singles, mutuals) = sccs.partition(_.size == 1)
      (singles.flatten, mutuals.flatten)
    }

    /**
     * Collect all ids that are transitively sunk (removed from toplevel).
     * Starts from toplevel functions and recurses into their sinkable children.
     */
    def collectAllSunk(parentId: Id): Set[Id] = {
      val (sinkable, _) = sinkableChildren(parentId)
      sinkable.toSet ++ sinkable.flatMap(collectAllSunk)
    }

    // Unsinkable children of toplevel functions must be promoted back to toplevel
    val promotedToToplevel: Set[Id] = toplevelIds.flatMap { tid =>
      val (_, unsinkable) = sinkableChildren(tid)
      unsinkable.toSet
    }

    val sunkIds: Set[Id] = toplevelIds.flatMap(collectAllSunk) -- promotedToToplevel

    // --- Step 4: Rebuild program ---

    /**
     * Build the body of a function, wrapping it with Stmt.Def for each
     * sinkable child (in order so that later defs can reference earlier ones).
     */
    def buildBody(fnId: Id, originalBody: Stmt): Stmt = {
      val (sinkable, _) = sinkableChildren(fnId)

      sinkable.foldRight(originalBody) { (childId, rest) =>
        val childDef = defMap(childId)
        val childBody = buildBody(childId, childDef.body)
        Stmt.Def(childId, childDef.params, childBody, rest)
      }
    }

    val newDefs = defs.collect {
      case d: ToplevelDefinition.Def if !sunkIds.contains(d.id) =>
        ToplevelDefinition.Def(d.id, d.params, buildBody(d.id, d.body))
    }

    m.copy(definitions = newDefs ++ rest)
  }

  // --- Dominator computation (Cooper-Harvey-Kennedy) ---

  private def computeDominators(
    nodes: List[Id],
    root: Id,
    preds: Map[Id, Set[Id]],
    rpo: Map[Id, Int]
  ): Map[Id, Id] = {

    val idom = mutable.Map.empty[Id, Id]
    idom(root) = root

    def intersect(b1: Id, b2: Id): Id = {
      var finger1 = b1
      var finger2 = b2
      while (finger1 != finger2) {
        while (rpo(finger1) > rpo(finger2)) finger1 = idom(finger1)
        while (rpo(finger2) > rpo(finger1)) finger2 = idom(finger2)
      }
      finger1
    }

    val ordered = nodes.sortBy(rpo).filterNot(_ == root)

    var changed = true
    while (changed) {
      changed = false
      for (b <- ordered) {
        val ps = preds.getOrElse(b, Set.empty).filter(idom.contains)
        if (ps.nonEmpty) {
          var newIdom = ps.head
          for (p <- ps.tail if idom.contains(p))
            newIdom = intersect(p, newIdom)
          if (!idom.get(b).contains(newIdom)) {
            idom(b) = newIdom
            changed = true
          }
        }
      }
    }

    idom.toMap
  }

  private def reversePostorder(root: Id, succs: Map[Id, Set[Id]], allNodes: Set[Id]): Map[Id, Int] = {
    val visited = mutable.Set.empty[Id]
    val order = mutable.ListBuffer.empty[Id]

    def dfs(n: Id): Unit = {
      if (visited.contains(n)) return
      visited += n
      for (s <- succs.getOrElse(n, Set.empty)) dfs(s)
      order.prepend(n)
    }

    dfs(root)
    for (n <- allNodes if !visited.contains(n)) order.append(n)
    order.toList.zipWithIndex.toMap
  }

  // --- Tarjan's SCC ---

  private def tarjanSCC(nodes: List[Id], succs: Map[Id, Set[Id]]): List[List[Id]] = {
    var index = 0
    val nodeIndex = mutable.Map.empty[Id, Int]
    val nodeLowlink = mutable.Map.empty[Id, Int]
    val onStack = mutable.Set.empty[Id]
    val stack = mutable.Stack.empty[Id]
    val result = mutable.ListBuffer.empty[List[Id]]

    def strongconnect(v: Id): Unit = {
      nodeIndex(v) = index
      nodeLowlink(v) = index
      index += 1
      stack.push(v)
      onStack += v

      for (w <- succs.getOrElse(v, Set.empty)) {
        if (!nodeIndex.contains(w)) {
          strongconnect(w)
          nodeLowlink(v) = math.min(nodeLowlink(v), nodeLowlink(w))
        } else if (onStack.contains(w)) {
          nodeLowlink(v) = math.min(nodeLowlink(v), nodeIndex(w))
        }
      }

      if (nodeLowlink(v) == nodeIndex(v)) {
        val scc = mutable.ListBuffer.empty[Id]
        var w = stack.pop()
        onStack -= w
        scc += w
        while (w != v) {
          w = stack.pop()
          onStack -= w
          scc += w
        }
        result += scc.toList
      }
    }

    for (n <- nodes if !nodeIndex.contains(n)) strongconnect(n)
    result.toList
  }
}
