package kiama
package util

/**
 * Implementation of Ropes as described in Boehm et al.'s "Ropes: an Alternative to Strings" (1995).
 *
 * A Rope is a tree structure for representing strings that enables efficient insertion, deletion, and
 * concatenation operations. Unlike traditional string implementations that store characters in
 * contiguous arrays, Ropes represent strings as binary trees where:
 *
 * - Leaf nodes contain string fragments
 * - Internal nodes (Concat) concatenate their children
 * - The full string is the in-order traversal of the leaves
 *
 * This implementation uses eager evaluation for simplicity, which affects the complexity bounds
 * compared to the lazy evaluation described in the original paper.
 *
 * Key operations and complexities:
 * - charAt(i): O(log n) - Binary search through the tree using length fields
 * - concat(rope1, rope2): O(1) for the operation, but materialization later is O(n)
 * - substring(start, end): O(n) - May need to copy characters
 * - iterator: O(n) - Left-to-right tree traversal
 * - toString: O(n) - Materializes the full string
 * - insert(index, str): O(n) - Requires substring operations
 * - delete(start, end): O(n) - Also requires substring operations
 *
 * Implementation notes:
 * 1. Uses a fixed leaf threshold rather than Fibonacci-based rebalancing
 * 2. No function nodes - Focus on core string operations needed for LSP
 * 3. No lazy evaluation - Operations are eager for simpler reasoning about updates
 *
 * Memory considerations:
 * Ropes require more memory per character than contiguous strings but enable:
 * - O(1) concatenation without immediate copying
 * - Incremental updates without always copying the entire string
 */
sealed trait Rope {
  /** Returns the total length of the string represented by this rope. O(1) */
  def length: Int

  /** Returns the character at the specified index. O(log n) */
  def charAt(index: Int): Char

  /** Concatenates this rope with another. O(1) for the operation, O(n) for eventual materialization */
  def concat(other: Rope): Rope

  /** Returns a new rope representing the substring from start (inclusive) to end (exclusive). O(n) */
  def substring(start: Int, end: Int): Rope

  /** Inserts a string at the specified index. O(n) */
  def insert(index: Int, str: String): Rope

  /** Deletes the substring from start (inclusive) to end (exclusive). O(n) */
  def delete(start: Int, end: Int): Rope

  /** Returns an iterator over the characters in the rope. Full iteration is O(n) */
  def iterator: Iterator[Char]

  /** Returns the string representation of this rope. O(n) */
  def toString: String
}

case class Leaf(text: String) extends Rope {
  def length: Int = text.length

  def charAt(index: Int): Char = {
    require(index >= 0 && index < length)
    text(index)
  }

  def concat(other: Rope): Rope = Rope.concat(this, other)

  def substring(start: Int, end: Int): Rope = {
    require(start >= 0 && end <= length && start <= end)
    if (start == 0 && end == length) this
    else Leaf(text.substring(start, end))
  }

  def insert(index: Int, str: String): Rope = {
    require(index >= 0 && index <= length)
    if (str.isEmpty) this
    else Leaf(text.substring(0, index) + str + text.substring(index))
  }

  def delete(start: Int, end: Int): Rope = {
    require(start >= 0 && end <= length && start <= end)
    if (start == end) this
    else Leaf(text.substring(0, start) + text.substring(end))
  }

  def iterator: Iterator[Char] = text.iterator

  override def toString: String = text
}

case class Concat(left: Rope, right: Rope) extends Rope {
  val length: Int = left.length + right.length

  def charAt(index: Int): Char = {
    require(index >= 0 && index < length)
    if (index < left.length) left.charAt(index)
    else right.charAt(index - left.length)
  }

  def concat(other: Rope): Rope = Rope.concat(this, other)

  def substring(start: Int, end: Int): Rope = {
    require(start >= 0 && end <= length && start <= end)
    if (start == 0 && end == length) this
    else if (end <= left.length) left.substring(start, end)
    else if (start >= left.length) right.substring(start - left.length, end - left.length)
    else Rope.concat(
      left.substring(start, left.length),
      right.substring(0, end - left.length)
    )
  }

  def insert(index: Int, str: String): Rope = {
    require(index >= 0 && index <= length)
    if (str.isEmpty) this
    else if (index <= left.length) Rope.concat(left.insert(index, str), right)
    else Rope.concat(left, right.insert(index - left.length, str))
  }

  def delete(start: Int, end: Int): Rope = {
    require(start >= 0 && end <= length && start <= end)
    if (start == end) this
    else if (end <= left.length) Rope.concat(left.delete(start, end), right)
    else if (start >= left.length) Rope.concat(left, right.delete(start - left.length, end - left.length))
    else Rope.concat(
      left.delete(start, left.length),
      right.delete(0, end - left.length)
    )
  }

  def iterator: Iterator[Char] = left.iterator ++ right.iterator

  override def toString: String = iterator.mkString
}

object Rope {
  /** Maximum size of a leaf node. Chosen based on empirical performance testing. */
  val LeafThreshold = 12

  val empty: Rope = Leaf("")

  def apply(str: String): Rope = {
    if (str.length <= LeafThreshold) Leaf(str)
    else {
      val mid = str.length / 2
      Concat(apply(str.substring(0, mid)), apply(str.substring(mid)))
    }
  }

  def concat(left: Rope, right: Rope): Rope = {
    (left, right) match {
      case (Leaf(l), Leaf(r)) if l.length + r.length <= LeafThreshold =>
        Leaf(l + r)
      case _ =>
        Concat(left, right)
    }
  }
}
