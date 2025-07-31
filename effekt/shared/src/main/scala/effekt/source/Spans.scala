package effekt.source

import effekt.source.Tree
import kiama.util.Position

object Spans {
  /**
   * Find all the given nodes whose range is in given `range`.
   */
  def findNodesContaining[T <: Tree](nodes: Vector[T], position: Position): Vector[T] = {
    nodes.filter { t =>
      val r = t.span.range
      position.between(r.from, r.to)
    }
  }

  /**
   * Find all the given nodes whose entire span is within the given `range`.
   */
  def findNodesInRange[T <: Tree](nodes: Vector[T], range: kiama.util.Range): Vector[T] =
    nodes.filter { t =>
      val r = t.span.range
      r.from.between(range.from, range.to) && (range.from <= r.to) && (r.to <= range.to)
    }


  /**
   * Get the source text associated with the substring of a source
   * between given starting and finishing positions. The two positions
   * are assumed to reference the same source. If either of the
   * positions doesn't refer to a valid offset in the source then
   * `None` is returned.
   */
  def substring(s: Position, f: Position): Option[String] =
    (s.optOffset, f.optOffset) match {
      case (Some(soffset), Some(foffset)) =>
        Some(s.source.content.substring(soffset, foffset))
      case _ =>
        None
    }
}
