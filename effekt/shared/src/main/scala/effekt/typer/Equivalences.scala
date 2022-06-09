package effekt
package typer

import effekt.symbols.{ UnificationVar, ValueType, TypeVar, CaptUnificationVar, CaptureSet }

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
  private var substitution: Map[Node, ValueType] = Map.empty,

  /**
   * A map from a member in the equivalence class to the class' representative
   */
  private var classes: Map[UnificationVar, Node] = Map.empty
) {

  // The current substitutions
  def subst: Map[TypeVar, ValueType] =
    classes.flatMap[TypeVar, ValueType] { case (k, v) => substitution.get(v).map { k -> _ } }.toMap

  def updateWith(captureSubstitution: Map[CaptUnificationVar, CaptureSet]): Unit =
    val subst = Substitutions(Map.empty, captureSubstitution.asInstanceOf)
    substitution = substitution.map { case (n, tpe) => n -> subst.substitute(tpe) }

  /**
   * Should only be called on unification variables where we do not know any types, yet
   *
   * It will *not* compare the types, but only equality imposed by constraints.
   */
  def isEqual(x: UnificationVar, y: UnificationVar): Boolean =
    getNode(x) == getNode(y)

  def typeOf(x: UnificationVar): Option[ValueType] =
    typeOf(getNode(x))

  def learn(x: UnificationVar, y: ValueType)(merge: (ValueType, ValueType) => Unit): Unit = {

    def learnType(x: Node, tpe: ValueType): Unit = {
      assert(!tpe.isInstanceOf[UnificationVar])
      typeOf(x) foreach { otherTpe => merge(tpe, otherTpe) }
      substitution = substitution.updated(x, tpe)

      // Now update substitution by applying it to itself.
      // This way we replace {?B} -> Box[?R] with {?B} -> Box[Int] when learning ?B =:= Int
      val updatedSubst = subst
      substitution = substitution.view.mapValues(tpe => updatedSubst.substitute(tpe)).toMap
    }

    def connectNodes(x: Node, y: Node): Unit = {
      // Already connected
      if (x == y) return ()

      (typeOf(x), typeOf(y)) match {
        case (Some(typeOfX), Some(typeOfY)) => merge(typeOfX, typeOfY)
        case (Some(typeOfX), None) => learnType(y, typeOfX)
        case (None, Some(typeOfY)) => learnType(x, typeOfY)
        case (None, None) => ()
      }

      // create mapping to representative
      classes = classes.view.mapValues { node => if (node == x) y else node }.toMap
    }

    y match {
      case y: UnificationVar => connectNodes(getNode(x), getNode(y))
      case tpe => learnType(getNode(x), tpe)
    }
  }

  def dumpConstraints() =
    println("\n--- Constraints ---")
    val cl = classes.groupMap { case (el, repr) => repr } { case (el, repr) => el }
    cl foreach {
      case (n, vars) => substitution.get(n) match {
        case None => println(s"{${vars.mkString(", ")}}")
        case Some(tpe) => println(s"{${vars.mkString(", ")}} --> ${tpe}")
      }
    }
    println("------------------\n")

  def allDefined(): Boolean =
    classes.values.forall { cl => substitution.isDefinedAt(cl) }

  override def clone(): Equivalences = new Equivalences(substitution, classes)

  private def getNode(x: UnificationVar): Node =
    classes.getOrElse(x, { val rep = new Node; classes += (x -> rep); rep })

  private def typeOf(n: Node): Option[ValueType] =
    substitution.get(n)

  /**
   * Given a representative gives the list of all unification variables it is known to
   * be in the same equivalence class with.
   */
  private def variablesFor(representative: Node): Set[UnificationVar] =
    val transposed = classes.groupMap { case (el, repr) => repr } { case (el, repr) => el }
    transposed.getOrElse(representative, Nil).toSet
}