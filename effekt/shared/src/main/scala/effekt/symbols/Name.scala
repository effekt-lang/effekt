package effekt
package symbols

import source.Id


sealed trait Name {

  def name: String

  override def toString = name
}

// Pseudo-constructors to safely convert ids and strings into names.
object Name {

  def local(id: Id): Name = ???


}
