package effekt
package cpsds

import effekt.core.Id
import scala.collection.immutable.IntMap

/**
 * Effectively just a Map[Id, T]
 *
 * Stores values in an IntMap with the hope that `IntMap.unionWith` is faster.
 * Additionally, `Id`s are stored separately to restore the illusion of a Map[Id, T]
 */
case class DB[T](values: IntMap[T], keys: Set[Id]) {
  def +(kv: (Id, T)): DB[T] =
    DB(values.updated(kv._1.id, kv._2), keys + kv._1)

  def getOrElse(key: Id, default: => T): T =
    values.getOrElse(key.id, default)

  def unionWith(other: DB[T], join: (T, T) => T): DB[T] =
    DB(values.unionWith(other.values, { (_, v1, v2) => join(v1, v2) }), keys ++ other.keys)

  def ++(other: DB[T]): DB[T] =
    DB(values ++ other.values, keys ++ other.keys)

  def mapValues[R](p: T => R): DB[R] =
    DB(values.map { case (k, v) => k -> p(v) }, keys)

  def mapWithId[R](p: (Id, T) => R): DB[R] =
    DB(values.map { case (k, v) => k -> p(Id("$", k), v) }, keys)

  def debug: Map[Id, T] = {
    val keyMap: Map[Int, Id] = keys.map(k => k.id -> k).toMap
    values.map { case (k, v) => keyMap(k) -> v }
  }
}
object DB {
  def empty[T]: DB[T] = DB(IntMap.empty, Set.empty)
  def apply[T](id: Id, value: T): DB[T] = DB(IntMap.singleton(id.id, value), Set(id))
}
