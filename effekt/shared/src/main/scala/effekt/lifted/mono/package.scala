package effekt
package lifted
package mono

/**
 * One entry in one particular evidence:
 *
 *     <α17._2, Try, Try>
 *              ^^^
 */
enum Lift {
  case Try()
  case Reg()
  case Var(x: Evidences.FlowVar, selector: Int)

  def show: String = this match {
    case Try() => "Try"
    case Reg() => "Reg"
    case Var(x, selector) => x.show + "._" + selector
  }
}

case class Ev(lifts: List[Lift]) {
  def show: String =  s"<${lifts.map(_.show).mkString(", ")}>"
}
object Ev {
  val Zero = Ev(Nil)
  def zero(arity: Int): Evidences.Concrete = Evidences.Concrete((0 until arity).map(_ => Zero).toList)
}

// evidences on a function are either a variable or a vector of concrete evidences
enum Evidences {
  // e.g. [<α17._2, Try, Try>, <>]
  case Concrete(evs: List[Ev])
  // e.g. α17
  case FlowVar(id: Int, arity: Int)

  def show: String = this match {
    case Evidences.Concrete(evs) => s"[${evs.map(_.show).mkString(", ")}]"
    case Evidences.FlowVar(id, arity) => s"α${id}"
  }
}
object Evidences {
  var last = -1
  def fresh(arity: Int): Evidences.FlowVar =
    last += 1
    Evidences.FlowVar(last, arity)
}

enum FlowType {
  // Structural (we ignore value parameters and return for now since we disregard boxing)
  case Function(evidences: Evidences, bparams: List[FlowType])

  // Nominal (there exists one such type for each id)
  case Interface(id: Id)

  def show: String = this match {
    case FlowType.Function(evs, bparams) => s"${evs.show}(${bparams.map(_.show).mkString(", ")})"
    case FlowType.Interface(id) => id.toString
  }
}

case class InterfaceDeclaration(operations: Map[Id, FlowType]) {
  def show: String = {
    val prettyOperations = operations.map { case (id, tpe) => id.toString + ": " + tpe.show  }.mkString(", ")
    s"{ ${prettyOperations} }"
  }
}
