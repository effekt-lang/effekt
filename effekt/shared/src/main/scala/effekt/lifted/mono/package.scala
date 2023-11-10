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
  case Reg() // used for local variables AND region based state.
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
  case FlowVar(id: Int, arity: Int, origin: Any)

  def show: String = this match {
    case Evidences.Concrete(evs) => s"[${evs.map(_.show).mkString(", ")}]"
    case Evidences.FlowVar(id, arity, origin) => s"α${id}"
  }

  override def toString: String = this match {
    case Concrete(evs) => s"Concrete(${evs.mkString(", ")})"
    case FlowVar(id, arity, _) => s"FlowVar(${id}, ${arity})"
  }

  override def equals(other: Any): Boolean = (this, other) match {
    case (Concrete(evs1), Concrete(evs2)) => evs1 == evs2
    case (FlowVar(id1, _, _), FlowVar(id2, _, _)) => id1 == id2
    case _ => false
  }

  override def hashCode(): Int = this match {
    case Concrete(evs) => 13 * evs.hashCode
    case FlowVar(id, _, _) => 17 * id.hashCode
  }
}
object Evidences {
  var last = -1
  def fresh(arity: Int, origin: Any): Evidences.FlowVar =
    last += 1
    Evidences.FlowVar(last, arity, origin)
}

enum FlowType {
  // Structural (we ignore value parameters and return for now since we disregard boxing)
  case Function(evidences: Evidences.FlowVar, tparams: List[Id], vparams: List[ValueType], bparams: List[FlowType], result: ValueType)

  // Nominal (there exists one such type for each id)
  case Interface(id: Id, targs: List[ValueType])

  def show: String = this match {
    case FlowType.Function(evs, _, _, bparams, _) => s"${evs.show}(${bparams.map(_.show).mkString(", ")})"
    case FlowType.Interface(id, _) => id.toString
  }
  override def equals(other: Any): Boolean = (this, other) match {
    case (f1: FlowType.Function, f2: FlowType.Function) =>
      f1.evidences == f2.evidences && (f1.bparams zip f2.bparams).forall(_ == _)
    case (i1: FlowType.Interface, i2: FlowType.Interface) =>
      i1.id == i2.id
    case _ => false
  }

  override def hashCode(): Int = this match {
    case FlowType.Function(ev, tps, vps, bps, res) => ev.hashCode() + 1337 * bps.hashCode()
    case FlowType.Interface(id, targs) => id.hashCode()
  }
}

case class InterfaceDeclaration(operations: Map[Id, FlowType]) {
  def show: String = {
    val prettyOperations = operations.map { case (id, tpe) => id.toString + ": " + tpe.show  }.mkString(", ")
    s"{ ${prettyOperations} }"
  }
}
