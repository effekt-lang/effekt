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

import kiama.relation.Relation.emptyImage
import kiama.util.Memoiser
import kiama.util.Memoiser.makeIdMemoiser

/**
 * A tree relation is a binary relation on tree nodes with an extra property
 * that the `image` operation throws a `NodeNotInTreeException` exception if
 * it is applied to a node that is not in this tree. `T` is the type of the
 * tree nodes.
 */
class TreeRelation[T <: AnyRef with Product](
  tree: Tree[T, _ <: T],
  override val graph: Memoiser[T, Vector[T]] = makeIdMemoiser[T, Vector[T]](),
  override val inverseGraph: Memoiser[T, Vector[T]] = makeIdMemoiser[T, Vector[T]]()
) extends Relation[T, T](graph, inverseGraph) {

  rel =>

  // Make sure root is present
  graph.putIfAbsent(tree.root, emptyImage)
  inverseGraph.putIfAbsent(tree.root, emptyImage)

  /**
   * Set the image of the relation for `t` to `us`. Takes care to make
   * sure that all mentioned nodes are present in the graph and the
   * inverse graph.
   */
  def set(t: T, us: Vector[T]): Unit = {
    graph.updateAt(t, _ ++ us, us)
    for (u <- us)
      graph.putIfAbsent(u, emptyImage)
    inverseGraph.putIfAbsent(t, emptyImage)
    for (u <- us)
      inverseGraph.updateAt(u, _ :+ t, Vector(t))
  }

  override def apply(t: T): Vector[T] = {
    val v = super.getGraph(t)
    if (v == null)
      throw new NodeNotInTreeException(t)
    else
      v
  }

  override lazy val inverse: TreeRelation[T] =
    new TreeRelation[T](tree, inverseGraph, graph) {
      override lazy val inverse: TreeRelation[T] =
        rel
    }

}

/**
 * Support for tree relations.
 */
object TreeRelation {

  import scala.annotation.tailrec
  import scala.collection.immutable.Queue

  /**
   * Return whether this node is a leaf node or not.
   */
  def isLeaf[T <: Product](t: T): Boolean = {
    for (desc <- t.productIterator) {
      desc match {
        case _: Option[_] | _: Either[_, _] | _: Tuple1[_] |
          _: Tuple2[_, _] | _: Tuple3[_, _, _] | _: Tuple4[_, _, _, _] =>
        // Do nothing
        case _: Product =>
          return false
        case _ =>
        // Do nothing
      }
    }
    true
  }

  /**
   * Return a vector of the children of `t`, skipping values that do not
   * contribute directly to the tree structure. See the documentation of the
   * `Tree` class for a detailed explanation of values that are skipped by
   * this method.
   */
  def treeChildren[T <: Product](t: T): Vector[T] = {

    @tailrec
    def loop(pending: Queue[Any], children: Vector[T]): Vector[T] =
      if (pending.isEmpty)
        children
      else {
        val candidate = pending.front
        val rest = pending.tail
        candidate match {
          case _: Bridge[_] =>
            // ignore
            loop(rest, children)

          case Some(n) =>
            loop(n +: rest, children)
          case None =>
            // ignore
            loop(rest, children)

          case Left(l) =>
            loop(l +: rest, children)
          case Right(r) =>
            loop(r +: rest, children)

          case Tuple1(a) =>
            loop(a +: rest, children)
          case (a, b) =>
            loop(List(a, b) ++: rest, children)
          case (a, b, c) =>
            loop(List(a, b, c) ++: rest, children)
          case (a, b, c, d) =>
            loop(List(a, b, c, d) ++: rest, children)

          case s: TraversableOnce[_] =>
            loop(s ++: rest, children)

          case p: Product =>
            loop(rest, children :+ (p.asInstanceOf[T]))

          case _ =>
            // ignore
            loop(rest, children)
        }
      }

    loop(Queue(t.productIterator), emptyImage)

  }

  /**
   * Make a child tree relation for the given tree. Populate the relation
   * using `treeChildren` to traverse the structure from the tree's root.
   */
  def childFromTree[T <: AnyRef with Product, R <: T](tree: Tree[T, R]): TreeRelation[T] = {

    val relation = new TreeRelation(tree)

    @tailrec
    def loop(pending: Queue[T]): TreeRelation[T] =
      if (pending.isEmpty)
        relation
      else {
        val l = pending.front
        val next = treeChildren(l)
        if (!next.isEmpty)
          relation.set(l, next)
        loop(pending.tail ++ next)
      }

    loop(Queue(tree.root))

  }

}
