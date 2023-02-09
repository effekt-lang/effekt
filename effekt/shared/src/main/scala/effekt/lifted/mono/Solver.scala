package effekt
package lifted
package mono

// {} <: _ <: { [<>], [<Try>] }
case class Bounds(lower: Set[Evidences], upper: Set[Evidences])

// TODO always add ?main <:< [<>]
//  we could also assume that all functions that do not have bounds, are bounded with [<>]

type Bisubstitution = Map[Evidences.FlowVar, Bounds]

case class Solver(constraints: List[Constraint], seen: Set[Constraint], subst: Bisubstitution) {
  def step(): Solver = constraints match {
    case head :: tail if seen contains head => Solver(tail, seen, subst)

    case Constraint.B(i1: FlowType.Interface, i2: FlowType.Interface) :: rest =>
      assert(i1 == i2, s"The two interfaces are not the same! ${i1} and ${i2}")
      Solver(rest, seen, subst)

    case (c @ Constraint.B(FlowType.Function(ev1, _, _, bparams1, _), FlowType.Function(ev2, _, _, bparams2, _))) :: rest =>
      val evidenceFlow = Constraint.E(ev1, ev2)
      val paramsFlow = bparams2.zip(bparams1).map { case (c2, c1) => Constraint.B(c2, c1) }
      Solver(evidenceFlow :: paramsFlow ++ rest, seen + c, subst)

    case Constraint.E(Evidences.Concrete(evs1), Evidences.Concrete(evs2)) :: rest =>
      Solver(rest, seen, subst)

    case (c @ Constraint.E(x: Evidences.FlowVar, y)) :: rest =>
      val xbounds = subst.getOrElse(x, Bounds(Set.empty, Set.empty))
      Solver(xbounds.lower.map(Constraint.E(_, y)).toList ++ rest, seen + c, subst + (x -> Bounds(xbounds.lower, xbounds.upper + y)))

    case (c @ Constraint.E(x, y: Evidences.FlowVar)) :: rest =>
      val ybounds = subst.getOrElse(y, Bounds(Set.empty, Set.empty))
      Solver(ybounds.upper.map(Constraint.E(x, _)).toList ++ rest, seen + c, subst + (y -> Bounds(ybounds.lower + x, ybounds.upper)))

    case c :: rest =>
      sys error s"Mismatch: ${c}"

    case Nil => this
  }
}

def solve(cs: List[Constraint]): Map[Evidences.FlowVar, Bounds] =
  var solver = Solver(cs, Set.empty, Map.empty)
  while (solver.constraints.nonEmpty) {
    solver = solver.step()
  }
  solver.subst

// a1 <: [<a2.0>]
// a2 <: [<>]


// A very naive implementation, that is quadradic in the number of unification variables
// it also does not detect "stack shape polymorphic recursion", which needs to be implemented separately.
def substitution(from: List[(Evidences.FlowVar, Bounds)]): Bisubstitution = from match {
  case (x, bounds) :: rest =>
    val subst = substitution(rest)
    val updatedBounds = Substitution(subst).substitute(bounds)
    substitute(x, updatedBounds, subst) + (x -> updatedBounds)
  case Nil => Map.empty
}

def substitute(x: Evidences.FlowVar, bounds: Bounds, into: Bisubstitution): Bisubstitution =
  into.map { case (y, ybounds) => y -> substitute(x, bounds, ybounds) }

def substitute(x: Evidences.FlowVar, bounds: Bounds, into: Bounds): Bounds =
  Bounds(substitute(x, bounds.lower, into.lower), substitute(x, bounds.upper, into.upper))

def substitute(x: Evidences.FlowVar, choices: Set[Evidences], into: Set[Evidences]): Set[Evidences] =
  into.flatMap(evs => substitute(x, choices, evs))

// [<>, <a1.0>] [a1 !-> { [<>], [<Try>] }]  =  [<>, <>],  [<>, <Try>]
def substitute(x: Evidences.FlowVar, choices: Set[Evidences], into: Evidences): Set[Evidences] = into match {
  case Evidences.Concrete(evs) => choices.map { c =>
    Evidences.Concrete(evs.map { ev => substituteSingleChoice(x, c, ev) })
  }
  case y : Evidences.FlowVar if x == y => choices
  case y : Evidences.FlowVar => Set(y)
}

// <Try, ?a.0>[?a !-> [<Try>, <>]]  = <Try, <Try>> = <Try, Try>
def substituteSingleChoice(x: Evidences.FlowVar, ev: Evidences, into: Ev): Ev =
  Ev(into.lifts.flatMap(l => substituteSingleChoice(x, ev, l)))

def substituteSingleChoice(x: Evidences.FlowVar, ev: Evidences, into: Lift): List[Lift] = into match {
  case Lift.Var(y, selector) if x == y => ev match {
    case Evidences.Concrete(evs) => evs(selector).lifts
    case z : Evidences.FlowVar => List(Lift.Var(z, selector))
  }
  case other => List(other)
}


def freeVars(l: Lift): Set[Evidences.FlowVar] = l match {
  case Lift.Var(x, selector) => Set(x)
  case _ => Set.empty
}

def freeVars(ev: Ev): Set[Evidences.FlowVar] = ev.lifts.toSet.flatMap(freeVars)
def freeVars(evs: Evidences): Set[Evidences.FlowVar] = evs match {
  case Evidences.Concrete(evs) => evs.toSet.flatMap(freeVars)
  case x : Evidences.FlowVar => Set(x)
}

// Parallel substitution
// TODO we probably do not need to substitute into the lower bounds, since we never use them.
class Substitution(subst: Bisubstitution) {

  def substitute(into: Bisubstitution): Bisubstitution =
    into.map { case (y, ybounds) => y -> substitute(ybounds) }

  def substitute(into: Bounds): Bounds =
    Bounds(substitute(into.lower, false), substitute(into.upper, true))

  def substitute(into: Set[Evidences], upper: Boolean): Set[Evidences] =
    into.flatMap(evs => substitute(evs, upper))

  // [<>, <a1.0>] [a1 !-> { [<>], [<Try>] }]  =  [<>, <>],  [<>, <Try>]
  def substitute(into: Evidences, upper: Boolean): Set[Evidences] = into match {
    case Evidences.Concrete(evs) =>
      val free = freeVars(into)
      val defined = free intersect subst.keySet
      var result = Set(evs)
      // now we have to apply all substitutions
      defined.foreach { x =>
        val bounds = if (upper) subst(x).upper else subst(x).lower
        result = for {
          choice <- bounds
          evs <- result
        } yield evs.map(ev => substituteSingleChoice(x, choice, ev))
      }
      result.map(Evidences.Concrete.apply)

    case y : Evidences.FlowVar if subst.isDefinedAt(y) =>
      if (upper) subst(y).upper else subst(y).lower
    case y : Evidences.FlowVar => Set(y)
  }
}


def isZero(evs: Evidences): Boolean = evs match {
  case Evidences.Concrete(evs) => evs.forall(ev => ev.lifts.isEmpty)
  case Evidences.FlowVar(id, arity) => true
}

// post processing step: drop all bounds that still mention unification variables
// we drop all bindings for unification variables with empty bounds or ONLY zero evidence
def cleanup(subst: Bisubstitution): Bisubstitution = subst.map {
  case (x, Bounds(lower, upper)) =>
    x -> Bounds(lower.filter(e => freeVars(e).isEmpty), upper.filter(e => freeVars(e).isEmpty))
} filterNot {
  case (x, Bounds(lower, upper)) => lower.forall(isZero) && upper.forall(isZero)
}