package effekt

import effekt.context.Context
import effekt.symbols.{ FunctionType, InterfaceType, BoxedType, BlockType, RigidVar, Type, TypeApp, TypeVar, ValueType, CaptVar, CaptureSet }
import effekt.symbols.builtins.THole
import effekt.util.messages.ErrorReporter

object substitutions {

  // TODO implement bi-substitution for capture sets and substitution for type vars?
  case class Substitution(
    typeSubst: Map[TypeVar, ValueType], // maps a type variable to the inferred value type
    captSubst: Map[CaptVar, (CaptureSet, CaptureSet)] // maps a capture variable to its lower and upper bounds
  )

  type Substitutions = Map[TypeVar, ValueType]

  // TODO add back, but as inequalities
  trait RegionEq

  // Substitution is independent of the unifier
  implicit class SubstitutionOps(substitutions: Map[TypeVar, ValueType]) {
    def substitute(t: ValueType): ValueType = t match {
      case x: TypeVar =>
        substitutions.getOrElse(x, x)
      case TypeApp(t, args) =>
        TypeApp(t, args.map { substitute })
      case BoxedType(tpe) =>
        BoxedType(substitute(tpe))
      case other => other
    }

    // def substitute(e: Effects): Effects = Effects(e.toList.map(substitute))

    //    def substitute(e: Effect): Effect = e match {
    //      case EffectApp(e, tpes) => EffectApp(substitute(e), tpes.map(substitute))
    //      case e                  => e
    //    }

    def substitute(t: BlockType): BlockType = t match {
      // TODO for now substitution doesn't do anything on capability types.
      case b: InterfaceType => b
      case b: FunctionType  => substitute(b)
    }

    def substitute(t: FunctionType): FunctionType = t match {
      case FunctionType(tps, vps, bps, ret) =>
        // do not substitute with types parameters bound by this function!
        val substWithout = substitutions.filterNot { case (t, _) => tps.contains(t) }
        FunctionType(tps, vps map substWithout.substitute, bps map substWithout.substitute, substWithout.substitute(ret))
    }
  }

  sealed trait UnificationResult {
    def add(k: TypeVar, v: ValueType): UnificationResult
    def union(other: UnificationResult): UnificationResult
    def checkFullyDefined(rigids: List[RigidVar]): UnificationResult
    def getUnifier(implicit error: ErrorReporter): Unifier
  }

  case class Unifier(substitutions: Map[TypeVar, ValueType], constraints: Set[RegionEq] = Set.empty) extends UnificationResult {

    def getUnifier(implicit error: ErrorReporter): Unifier = this

    def addAll(unifier: Map[TypeVar, ValueType]): Unifier =
      Unifier(substitutions ++ unifier, constraints)

    def add(k: TypeVar, v: ValueType): UnificationResult = {
      substitutions.get(k).foreach { v2 =>
        if (v != v2) {
          return UnificationError(s"${k} cannot be instantiated with ${v} and with ${v2} at the same time.")
        }
      }

      // Use new substitution binding to refine right-hand-sides of existing substitutions.
      // Do we need an occurs check?
      val newSubst = Map(k -> v)
      val improvedSubst: Substitutions = substitutions.map { case (rigid, tpe) => (rigid, newSubst substitute tpe) }
      Unifier(improvedSubst + (k -> improvedSubst.substitute(v)), constraints)
    }

    def union(other: UnificationResult): UnificationResult = other match {
      case Unifier(subst, constr) =>
        val bothConstraints: UnificationResult = Unifier(subst, constr ++ constraints)
        substitutions.foldLeft(bothConstraints) { case (u, (k, v)) => u.add(k, v) }
      case err: UnificationError => err
    }

    def checkFullyDefined(rigids: List[RigidVar]): UnificationResult = {
      rigids.foreach { tpe =>
        if (!substitutions.isDefinedAt(tpe))
          return UnificationError(s"Couldn't infer type for ${tpe.underlying}")
      }
      this
    }

    def skolems(rigids: List[RigidVar]): List[RigidVar] =
      rigids.filterNot { substitutions.isDefinedAt }
  }
  object Unifier {
    def empty: Unifier = Unifier(Map.empty[TypeVar, ValueType])
    def apply(unify: (TypeVar, ValueType)): Unifier = Unifier(Map(unify))
  }
  case class UnificationError(msg: String) extends UnificationResult {
    // TODO currently we only return an empty unifier for backwards compatibility
    def getUnifier(implicit error: ErrorReporter): Unifier = {
      error.error(msg)
      Unifier.empty
    }
    def add(k: TypeVar, v: ValueType) = this
    def union(other: UnificationResult) = this
    def checkFullyDefined(rigids: List[RigidVar]) = this
  }

  /**
   * Allows using a unifier for substitution
   */
  implicit def toSubstitution(u: Unifier): SubstitutionOps = new SubstitutionOps(u.substitutions)

  object Unification {

    /**
     * For error reporting, we assume the second argument (tpe1) is the type expected by the context
     */
    def unify(tpe1: Type, tpe2: Type)(implicit C: ErrorReporter): Unifier =
      unifyTypes(tpe1, tpe2).getUnifier

    // The lhs can contain rigid vars that we can compute a mapping for
    // i.e. unify(List[?A], List[Int]) = Map(?A -> Int)
    def unifyTypes(tpe1: Type, tpe2: Type)(implicit C: ErrorReporter): UnificationResult =
      (tpe1, tpe2) match {

        case (t: ValueType, s: ValueType) =>
          unifyValueTypes(t, s)

        case (t: BlockType, s: BlockType) =>
          unifyBlockType(t, s)

        case (t, s) =>
          UnificationError(s"Expected ${t}, but got ${s}")
      }

    def unifyValueTypes(tpe1: ValueType, tpe2: ValueType)(implicit C: ErrorReporter): UnificationResult =
      (tpe1, tpe2) match {

        case (t, s) if t == s =>
          Unifier.empty

        case (s: RigidVar, t: ValueType) =>
          Unifier(s -> t)

        // occurs for example when checking the first argument of `(1 + 2) == 3` against expected
        // type `?R` (since `==: [R] (R, R) => Boolean`)
        case (s: ValueType, t: RigidVar) =>
          Unifier(t -> s)

        case (TypeApp(t1, args1), TypeApp(t2, args2)) if t1 == t2 =>
          if (args1.size != args2.size)
            return UnificationError(s"Argument count does not match $t1 vs. $t2")

          (args1 zip args2).foldLeft(Unifier.empty: UnificationResult) {
            case (u, (a1, a2)) => u union unifyValueTypes(a1, a2)
          }

        case (THole, _) | (_, THole) =>
          Unifier.empty

        case (BoxedType(tpe1), BoxedType(tpe2)) =>
          unifyTypes(tpe1, tpe2)

        case (t, s) =>
          UnificationError(s"Expected ${t}, but got ${s}")
      }

    def unifyBlockType(tpe1: BlockType, tpe2: BlockType)(implicit C: ErrorReporter): UnificationResult = (tpe1, tpe2) match {
      case (t: FunctionType, s: FunctionType) =>
        unifyFunctionType(t, s)

      case (t: InterfaceType, s: InterfaceType) =>
        unifyInterfaceType(t, s)

      case (t, s) =>
        UnificationError(s"Expected ${t}, but got ${s}")
    }

    def unifyInterfaceType(tpe1: InterfaceType, tpe2: InterfaceType)(implicit C: ErrorReporter): UnificationResult =
      // TODO implement properly
      if (tpe1 == tpe2) Unifier.empty
      else UnificationError(s"Expected ${tpe1}, but got ${tpe2}")

    def unifyFunctionType(tpe1: FunctionType, tpe2: FunctionType)(implicit C: ErrorReporter): UnificationResult =
      (tpe1, tpe2) match {
        // TODO also consider type parameters here
        case (f1 @ FunctionType(_, vargs1, bargs1, ret1), f2 @ FunctionType(_, vargs2, bargs2, ret2)) =>

          if (vargs1.size != vargs2.size)
            return UnificationError(s"Value argument count does not match $f1 vs. $f2")

          if (bargs1.size != bargs2.size)
            return UnificationError(s"Block argument count does not match $f1 vs. $f2")

          var unifier = unifyValueTypes(ret1, ret2)

          (vargs1 zip vargs2) foreach { case (a1, a2) => unifier = unifier union unifyValueTypes(a1, a2) }
          (bargs1 zip bargs2) foreach { case (a1, a2) => unifier = unifier union unifyBlockType(a1, a2) }

          unifier
      }

    // We don't unify effects here, instead we simply gather them
    // Two effects State[?X1] and State[?X2] are assumed to be disjoint until we know that ?X1 and ?X2 are equal.
    //    def unifyEffectful(e1: Effectful, e2: Effectful)(implicit C: Context): UnificationResult =
    //      unifyTypes(e1.tpe, e2.tpe)

    /**
     * Instantiate a typescheme with fresh, rigid type variables
     *
     * i.e. `[A, B] (A, A) => B` becomes `(?A, ?A) => ?B`
     */
    def instantiate(tpe: FunctionType)(implicit C: ErrorReporter): (List[RigidVar], FunctionType) = {
      val FunctionType(tparams, vparams, bparams, ret) = tpe
      val subst = tparams.map { p => p -> RigidVar(p) }.toMap
      val rigids = subst.values.toList

      val substitutedVparams = vparams map subst.substitute
      val substitutedBparams = bparams map subst.substitute
      val substitutedReturn = subst.substitute(ret)
      (rigids, FunctionType(Nil, substitutedVparams, substitutedBparams, substitutedReturn))
    }
  }
}
