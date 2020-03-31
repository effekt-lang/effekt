package effekt
package util

import scala.collection.mutable
import effekt.symbols.Symbol

/**
 * Something modeling a scope -- drop-in replacement for Kiama.Environments
 *
 * Scopes ares non-empty immutable linked lists storing mutable maps from symbols to T
 */
object scopes {

  sealed trait Scope[V] {

    def bindings: mutable.HashMap[String, V]

    def lookupHere(key: String): Option[V] = bindings.get(key)
    def lookup(key: String): Option[V] = lookupHere(key)

    def define(key: String, value: V): Unit = bindings.update(key, value)
    def enter: Scope[V] = BlockScope[V](mutable.HashMap.empty, this)
    def enterWith(bs: Map[String, V]) = BlockScope[V](mutable.HashMap.empty ++ bs, this)
    def leave: Scope[V] = sys error "Leaving top level scope"
  }
  case class Toplevel[V](bindings: mutable.HashMap[String, V]) extends Scope[V]
  case class BlockScope[V](bindings: mutable.HashMap[String, V], parent: Scope[V]) extends Scope[V] {
    override def lookup(key: String): Option[V] = super.lookup(key).orElse(parent.lookup(key))
    override def leave: Scope[V] = parent
  }

  def toplevel[V](bindings: Map[String, V]): Scope[V] = Toplevel(mutable.HashMap(bindings.toSeq: _*))
}
