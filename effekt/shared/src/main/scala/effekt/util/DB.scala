package effekt
package util


import effekt.core.Id

import scala.collection.immutable.IntMap

/**
 * Effectively just a Map[Id, T]
 *
 * Stores values in an IntMap with the hope that `IntMap.unionWith` is faster.
 * Additionally, `Id`s are stored separately to restore the illusion of a Map[Id, T]
 */
case class DB[+T](values: IntMap[T], keys: IntMap[Id]) {
  def get(id: Id): Option[T] = values.get(id.id)
  def isDefinedAt(id: Id): Boolean = values.isDefinedAt(id.id)
  def apply(id: Id): T = values(id.id)

  def keySet: Set[Id] = keys.keySet.map(keys)

  def +[S >: T](kv: (Id, S)): DB[S] =
    DB(values.updated(kv._1.id, kv._2), keys.updated(kv._1.id, kv._1))

  def getOrElse[S >: T](key: Id, default: => S): S =
    values.getOrElse(key.id, default)

  def unionWith[S >: T](other: DB[S], join: (S, S) => S): DB[S] =
    DB(values.unionWith(other.values, { (_, v1, v2) => join(v1, v2) }), keys.unionWith(other.keys, { (_, v1, v2) => v2 }))
  //
  //  def merged[S >: T](other: DB[S])(f: (ID, S, S) => S): DB[S] =
  //    DB(values.unionWith(other.values, { (_, v1, v2) => join(v1, v2) }), keys.unionWith(other.keys, { (_, v1, v2) => v2 }))

  def ++[S >: T](other: DB[S]): DB[S] = unionWith(other, (v1, v2) => v2)

  def -(id: Id): DB[T] = DB(values.removed(id.id), keys.removed(id.id))

  def removedAll(other: IterableOnce[Id]): DB[T] =
    var newValues = values
    var newKeys = keys
    other.iterator.foreach { id =>
      newValues = newValues.removed(id.id);
      newKeys = newKeys.removed(id.id)
    }
    DB(newValues, newKeys)

  def --(other: IterableOnce[Id]): DB[T] = removedAll(other)

  def mapValues[R](p: T => R): DB[R] =
    DB(values.map { case (k, v) => k -> p(v) }, keys)

  def mapWithId[R](p: (Id, T) => R): DB[R] =
    DB(values.map { case (k, v) => k -> p(keys(k), v) }, keys)

  /**
   * WARNING: this is slow and should be used with care (for debugging for instance)
   */
  def toMap: Map[Id, T] = {
    values.map { case (k, v) => keys(k) -> v }
  }

  def iterator: Iterator[(Id, T)] = values.iterator.map { case (k, v) => keys(k) -> v }
}
object DB {
  def empty[T]: DB[T] = DB(IntMap.empty, IntMap.empty)
  def apply[T](id: Id, value: T): DB[T] = DB(IntMap.singleton(id.id, value), IntMap.singleton(id.id, id))
  def apply[T](els: (Id, T)*): DB[T] = DB.from(els)
  def from[T](it: Iterable[(Id, T)]): DB[T] =
    var values = IntMap.empty[T]
    var keys = IntMap.empty[Id]
    it.foreach { case (id, value) =>
      values = values.updated(id.id, value)
      keys = keys.updated(id.id, id)
    }
    new DB[T](values, keys)
}
extension [T] (it: Iterable[(Id, T)]) {
  def toDB: DB[T] = DB.from(it)
}
