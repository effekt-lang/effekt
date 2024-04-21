package effekt
package context

import effekt.symbols.ResumeParam
import effekt.util.messages.ErrorReporter
import kiama.util.Memoiser

case class Annotation[K, V](name: String, description: String, bindToObjectIdentity: Boolean = true) {
  type Value = V
  override def toString = name
}

class Annotations private(
  /**
   * Local Annotations are organized differently to allow simple access.
   */
  private var annotations: Map[Annotation[_, _], Map[Annotations.Key[Any], Any]]
) {
  import Annotations._

}
object Annotations {

  def empty: Annotations = new Annotations(Map.empty)

  sealed trait Key[T] { def key: T }

  private def makeKey[K, V](ann: Annotation[K, V], k: K): Key[K] = ???

}

trait AnnotationsDB { self: Context =>

}
