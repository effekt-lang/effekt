package effekt
package typer

import effekt.symbols.{ UnificationVar, ValueType }

class Equivalences(
  /**
   * Everything we know so far -- right hand sides of substitutions are *always* concrete types.
   * They can, however, mention unification variables. e.g. `?U !-> List[?S]` is allowed.
   * The mentioned unification variables (`?S` in the example) must not have a substitution.
   * They can be part of an equivalence class.
   *
   * Once one member of the equivalence class becomes concrete, all members are assigned the same type
   * in the substitution.
   */
  private var substitution: Map[UnificationVar, ValueType] = Map.empty,

  /**
   * A map from a member in the equivalence class to the class' representative
   */
  private var classes: Map[UnificationVar, Node] = Map.empty
) {

  // The current substitutions
  def subst: Substitutions = new Substitutions(substitution.asInstanceOf, Map.empty)

  // should only be called on unification variables where we do not know any types, yet
  def isEqual(x: UnificationVar, y: UnificationVar): Boolean =
    getNode(x) == getNode(y)

  def typeOf(x: UnificationVar): Option[ValueType] = substitution.get(x)

  def learn(x: UnificationVar, y: ValueType)(merge: (ValueType, ValueType) => Unit): Unit =
   y match {

    case y: UnificationVar => (substitution.get(x), substitution.get(y)) match {
      // we already know a type for both; they better match
      case (Some(typeOfX), Some(typeOfY)) => merge(typeOfX, typeOfY)
      // We already know a type for x -- use the same for y
      case (Some(typeOfX), None) => learnType(y, typeOfX)
      // we already know a type for y -- use the same for x
      case (None, Some(typeOfY)) => learnType(x, typeOfY)

      // we do not know anything yet; put them into one equivalence class
      case (None, None) => connectNodes(getNode(x), getNode(y))
    }

    case tpe => substitution.get(x) match {
      // we already know a type -- they better match
      case Some(typeOfX) => merge(typeOfX, tpe)
      case None =>
        val typeOfX = subst.substitute(tpe)
        learnType(x, typeOfX)
    }
  }

  def dumpConstraints() =
    substitution foreach { case (x, tpe) => println(s"$x   !->   $tpe") }
    val cl = classes.groupMap { case (el, repr) => repr } { case (el, repr) => el }
    cl foreach {
      case (n, vars) => println(s"{${vars.mkString(", ")}}")
    }


  override def clone(): Equivalences = new Equivalences(substitution, classes)

  private def getNode(x: UnificationVar): Node =
    assert(!subst.isDefinedAt(x))
    classes.getOrElse(x, { val rep = new Node; classes += (x -> rep); rep })

  private def learnType(x: UnificationVar, tpe: ValueType): Unit =
    assert(!tpe.isInstanceOf[UnificationVar])
    val varsInClass = variablesFor(getNode(x))
    varsInClass foreach { y =>
      assert(!substitution.isDefinedAt(y))
      substitution = substitution.updated(x, tpe)
    }
    classes = classes.removedAll(varsInClass)


  /**
   * Given a representative gives the list of all unification variables it is known to
   * be in the same equivalence class with.
   */
  private def variablesFor(representative: Node): Set[UnificationVar] =
    val transposed = classes.groupMap { case (el, repr) => repr } { case (el, repr) => el }
    transposed.getOrElse(representative, Nil).toSet

  private def connectNodes(x: Node, y: Node): Unit =
    // Already connected
    if (x == y) return ()

    // create mapping to representative
    classes = classes.view.mapValues { node => if (node == x) y else node }.toMap

}