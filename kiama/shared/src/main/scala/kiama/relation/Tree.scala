/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2014-2020 Anthony M Sloane, Macquarie University.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package kiama
package relation

/**
 * Exception thrown if a tree relation operation tries to use a node that
 * is not in the tree to which the relation applies. `t` is the node that
 * was found not to be in the tree.
 *
 * For the time-being, the exception does not indicate which tree is involved
 * because it's not clear how to identify the tree briefly in a short message.
 * The stack trace that accompanies the exception should be sufficient.
 */
case class NodeNotInTreeException[T](t: T) extends Exception(s"node not in tree: $t")

/**
 * Exception thrown if `CheckTree` is specified as a tree's shape but the
 * provided structure is not a tree. `msg` describes the reason(s) why the
 * structure is not a tree by listing the nodes that have more than one
 * parent.
 */
case class StructureIsNotATreeException(msg: String) extends Exception(msg)

/**
 * A bridge node in a tree structure which connects to another structure.
 * Bridges are not traversed when determining the tree structure.
 */
case class Bridge[T](cross: T)

/**
 * Tree properties
 */
sealed abstract class TreeShape

/**
 * The incoming structure has an acceptable shape and should be left alone
 * by the tree processing.
 */
case object LeaveAlone extends TreeShape

/**
 * The incoming structure should be a tree (i.e., no sharing nor cycles) but
 * a runtime check should be performed to make sure.
 */
case object CheckTree extends TreeShape

/**
 * The incoming structure may not be a tree (e.g., may contain sharing) and
 * shared parts should be cloned to make it a tree before any further
 * processing is performed.
 */
case object EnsureTree extends TreeShape

/**
 * Relational representations of trees built out of hierarchical `Product`
 * instances. Typically, the nodes are instances of case classes.
 *
 * The type `T` is the common base type of the tree nodes. The type `R` is a
 * sub-type of `T` that is the type of the root node.
 *
 * The `originalRoot` value should be the root of a finite, acyclic structure
 * that contains only `Product` instances of type `T` (unless they are skipped,
 * see below).
 *
 * The `shape` argument governs how the structure referred to by `originalRoot`
 * is treated. If `shape` is `LeaveAlone` (the default) then the structure is
 * used without change. If `shape` is `CheckTree` then a check is run before
 * the structure is used to make sure that it is a tree (i.e., no node sharing
 * nor cycles). A runtime error occurs if this is not the case. If `shape` is
 * `EnsureTree` then shared parts of the structure are cloned to ensure that
 * it is a tree before it is used.
 *
 * If you are confident that your structure is a tree (e.g., it comes from a
 * parser) then the default shape should be fine. If you prefer a bit more
 * safety, then use `CheckTree` so that a non-tree structure cannot be missed.
 * Finally, use `EnsureTree` if you know that your structure may contain
 * sharing (e.g., it is produced by a term rewriting process).
 *
 * The `child` relation of a tree is defined to skip certain nodes.
 * Specifically, if a node of type `T` is wrapped in a `Some` of an option,
 * a `Left` or `Right` of an `Either`, a tuple up to size four, or a
 * `TraversableOnce`, then those wrappers are ignored. E.g., if `t1`
 * is `Some (t2)` where both `t1` and `t2` are of type `T`, then `t1`
 * will be `t2`'s parent, not the `Some` value.
 *
 * Thanks to Len Hamey for the idea to use lazy cloning to restore the tree
 * structure instead of requiring that the input trees contain no sharing.
 */
class Tree[T <: AnyRef with Product, +R <: T](val originalRoot: R, shape: TreeShape = LeaveAlone) {

  tree =>

  import kiama.relation.Relation.emptyImage
  import kiama.relation.TreeRelation
  import kiama.relation.TreeRelation.childFromTree
  import kiama.util.Comparison
  import kiama.util.Comparison.same

  /**
   * The root node of the tree. If this tree's `shape` argument is `EnsureTree`
   * the root node will be different from the original root if any nodes in
   * the original tree are shared, since they will be cloned as necessary
   * to yield a proper tree structure. If there is no sharing or `EnsureTree`
   * is not specified then `root` will be same as `originalRoot`.
   *
   * Bridges to other structures will not be traversed. This behaviour means
   * that you can have references to other structures while still processing
   * this structure as a tree in its own right.
   */
  lazy val root =
    originalRoot

  /**
   * The basic relations between a node and its children. All of the
   * other relations are derived from `child`. If this tree's shape
   * argument is `CheckTree` then a check will be performed that the
   * structure is actually a tree. A runtime error will be thrown
   * if it's not a tree.
   */
  lazy val child: TreeRelation[T] = {

    /*
         * The child relation for this tree.
         */
    val child = childFromTree(tree)

    // As a safety check, we make sure that values are not children
    // of more than one parent.
    if (shape == CheckTree) {
      val msgBuilder = new StringBuilder
      val parent = child.inverse

      def addAncestors(c: T, ps: Vector[T], level: Int = 1): Unit = {
        for (p <- ps) {
          val index = indexInVector(child(p), c)
          val indent = "  " * level
          msgBuilder ++= s"${indent}child $index of $p\n"
          addAncestors(p, parent(p), level + 1)
        }
      }

      for (c <- parent.domain) {
        val ps = parent(c)
        if (ps.length > 1) {
          msgBuilder ++= s"child $c has multiple parents:\n"
          addAncestors(c, ps)
        }
      }
      if (!msgBuilder.isEmpty)
        throw (new StructureIsNotATreeException(msgBuilder.result()))
    }

    // All ok
    child

  }

  /**
   * The nodes that occur in this tree. Mostly useful if you want to
   * iterate to look at every node.
   */
  lazy val nodes: Vector[T] =
    child.domain

  /**
   * If the tree contains node `u` return `v`, otherwise throw a
   * `NodeNotInTreeException`. `v` is only evaluated if necessary.
   */
  def whenContains[V](t: T, v: => V): V =
    if (same(t, root) || (parent.containsInDomain(t)))
      v
    else
      throw new NodeNotInTreeException(t)

  // Derived relations

  /**
   * Map the function `f` over the images of this tree's child relation and
   * use the resulting graph to make a new tree relation.
   */
  def mapChild(f: Vector[T] => Vector[T]): TreeRelation[T] = {
    val relation = new TreeRelation(tree)
    for (t <- child.domain)
      relation.set(t, f(child(t)))
    relation
  }

  /**
   * A relation that relates a node to its first child.
   */
  lazy val firstChild: TreeRelation[T] =
    mapChild(_.take(1))

  /**
   * A relation that relates a node to its last child.
   */
  lazy val lastChild: TreeRelation[T] =
    mapChild(_.takeRight(1))

  /**
   * The parent relation for this tree (inverse of child relation).
   */
  lazy val parent: TreeRelation[T] =
    child.inverse

  /**
   * A relation that relates a node to its next sibling. Inverse of
   * the `prev` relation.
   */
  lazy val next: TreeRelation[T] = {
    val relation = new TreeRelation(tree)
    relation.set(root, emptyImage)
    for (t <- child.domain) {
      val children = child(t)
      if (children.length > 0) {
        for (i <- 0 until children.length - 1)
          relation.set(children(i), Vector(children(i + 1)))
        relation.set(children(children.length - 1), emptyImage)
      }
    }
    relation
  }

  /**
   * A relation that relates a node to its previous sibling. Inverse of the
   * `next` relation.
   */
  lazy val prev: TreeRelation[T] =
    next.inverse

  /**
   * A relation that relates a node to its siblings, including itself.
   */
  lazy val sibling: TreeRelation[T] = {
    val relation = new TreeRelation(tree)
    relation.put(tree.root, tree.root)
    for (t <- parent.domain; p <- parent(t))
      relation.set(t, child(p))
    relation
  }

  // Predicates derived from the relations

  /**
   * Return the index of a child in a particular child vector.
   */
  def indexInVector(vector: Vector[T], t: T): Int =
    Comparison.indexOf(vector, t)

  /**
   * Return the first index of `t` in the children of `t's` parent node.
   * Counts from zero. Is zero for root.
   */
  def index(t: T): Int =
    parent(t) match {
      case Vector(p) =>
        indexInVector(child(p), t)
      case _ =>
        if (same(t, root))
          0
        else
          throw new NodeNotInTreeException(t)
    }

  /**
   * Return the last index of `t` in the children of `t's` parent node.
   * Counts from zero. Is zero for root.
   */
  def indexFromEnd(t: T): Int =
    parent(t) match {
      case Vector(p) =>
        val c = child(p)
        c.length - Comparison.lastIndexOf(c, t) - 1
      case _ =>
        if (same(t, root))
          0
        else
          throw new NodeNotInTreeException(t)
    }

  /**
   * Return whether or not `t` is a first child. True for root.
   */
  def isFirst(t: T): Boolean =
    prev(t).isEmpty

  /**
   * Return whether or not `t` is a last child. True for root.
   */
  def isLast(t: T): Boolean =
    next(t).isEmpty

  /**
   * Return whether or not `t` is the root of this tree.
   */
  def isRoot(t: T): Boolean =
    parent(t).isEmpty

}
