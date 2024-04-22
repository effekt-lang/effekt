package effekt
package context


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

  sealed trait Key[T] { def key: T }

}

trait AnnotationsDB { self: Context =>

}
