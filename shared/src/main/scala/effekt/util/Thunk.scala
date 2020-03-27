package effekt
package util

case class Thunk[+T](private val compute: () => T, private var _forced: Boolean) {
  lazy val value: T = { _forced = true; compute() }
  def forced = _forced
  override def toString =
    if (forced) s"Thunk($value)" else "Thunk(<not-forced>)"
}
object Thunk {
  def apply[T](f: => T): Thunk[T] = Thunk(() => f, false)
}
