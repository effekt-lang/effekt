package effekt
package util

import scala.collection.mutable

/**
 * Something modeling a scope -- drop-in replacement for Kiama.Environments
 *
 * Scopes ares non-empty immutable linked lists storing mutable maps from symbols to T
 */
object scopes {

  sealed trait Scope[K, V] {
    def bindings: mutable.HashMap[K, V]
    def lookup(key: K, err: => V): V = bindings.getOrElse(key, err)
    def define(key: K, value: V): Unit = bindings.update(key, value)
    def enter: Scope[K, V] = BlockScope[K, V](mutable.HashMap.empty, this)
    def enterWith(bs: Map[K, V]) = BlockScope[K, V](mutable.HashMap.empty ++ bs, this)
    def leave: Scope[K, V] = sys error "Leaving top level scope"
  }
  case class Toplevel[K, V](bindings: mutable.HashMap[K, V]) extends Scope[K, V]
  case class BlockScope[K, V](bindings: mutable.HashMap[K, V], parent: Scope[K, V]) extends Scope[K, V] {
    override def lookup(key: K, err: => V): V = super.lookup(key, parent.lookup(key, err))
    override def leave: Scope[K, V] = parent
  }

  def toplevel[K, V](bindings: Map[K, V]): Scope[K, V] = Toplevel(mutable.HashMap(bindings.toSeq: _*) )
}