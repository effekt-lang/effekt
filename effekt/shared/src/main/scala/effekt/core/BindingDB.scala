package effekt.core


import scala.collection.mutable.ListBuffer

/**
 * Storage for bindings
 */
final class BindingDB {

  /**
   * A _mutable_ ListBuffer that stores all bindings to be inserted at the current scope
   */
  var bindings: ListBuffer[Binding] = ListBuffer()
}
