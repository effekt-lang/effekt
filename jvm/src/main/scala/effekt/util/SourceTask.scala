package effekt
package util

import effekt.context.Context
import effekt.util.JavaPathUtils._

import scala.collection.mutable
import org.bitbucket.inkytonik.kiama.util.{ FileSource, Source, StringSource }

/**
 * A Caching wrapper around a phase, that invalidates the phase if the
 * source changed.
 */
class SourceTask[Out](phase: Task[Source, Out]) extends Task[Source, Out] {
  val taskName = phase.taskName

  type Timestamp = Long

  private val cache: mutable.Map[Source, (Timestamp, Option[Out])] = mutable.Map.empty

  def run(input: Source)(implicit C: Context): Option[Out] = {
    val (timestamp, res) = cache.getOrElse(input, (Long.MinValue, None))
    val changed = lastModified(input)

    if (changed > timestamp) {
      val result = phase.run(input)
      cache.update(input, (changed, result))
      result
    } else {
      res
    }
  }

  // it would be nice if this would be a method on kiama.Source
  def lastModified(src: Source): Timestamp = src match {
    case FileSource(name, encoding)  => file(name).lastModified
    case MarkdownSource(src)         => lastModified(src)

    // it is always 0 for string sources since they are compared by content
    case StringSource(content, name) => 0L
  }

  def reset(): Unit = cache.clear()

  //cache.getOrElseUpdate(input, phase.run(input))
}
