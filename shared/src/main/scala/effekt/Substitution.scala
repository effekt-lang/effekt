package effekt

import effekt.context.Context
import effekt.symbols.{ FunctionType, InterfaceType, BoxedType, BlockType, RigidVar, Sections, Type, TypeApp, TypeVar, ValueType }
import effekt.symbols.builtins.THole
import effekt.util.messages.ErrorReporter

object substitutions {

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
      case FunctionType(tps, ps, ret) =>
        val substWithout = substitutions.filterNot { case (t, _) => ps.contains(t) }
        FunctionType(tps, substWithout.substitute(ps), substWithout.substitute(ret))
    }

    def substitute(t: Sections): Sections = t map {
      _ map {
        case v: ValueType => substitute(v)
        case b: BlockType => substitute(b)
      }
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
    def unify(tpe1: Type, tpe2: Type)(implicit C: Context): Unifier =
      unifyTypes(tpe1, tpe2).getUnifier

    // The lhs can contain rigid vars that we can compute a mapping for
    // i.e. unify(List[?A], List[Int]) = Map(?A -> Int)
    def unifyTypes(tpe1: Type, tpe2: Type)(implicit C: Context): UnificationResult =
      (tpe1, tpe2) match {

        case (t: ValueType, s: ValueType) =>
          unifyValueTypes(t, s)

        case (t: BlockType, s: BlockType) =>
          unifyInterfaceTypes(t, s)

        case (t, s) =>
          UnificationError(s"Expected ${t}, but got ${s}")
      }

    def unifyValueTypes(tpe1: ValueType, tpe2: ValueType)(implicit C: Context): UnificationResult =
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

    def unifyInterfaceTypes(tpe1: BlockType, tpe2: BlockType)(implicit C: Context): UnificationResult = (tpe1, tpe2) match {
      case (t: FunctionType, s: FunctionType) =>
        unifyBlockTypes(t, s)

      case (t: InterfaceType, s: InterfaceType) =>
        unifyInterfaceType(t, s)

      case (t, s) =>
        UnificationError(s"Expected ${t}, but got ${s}")
    }

    def unifyInterfaceType(tpe1: InterfaceType, tpe2: InterfaceType)(implicit C: Context): UnificationResult =
      // TODO implement properly
      if (tpe1 == tpe2) Unifier.empty
      else UnificationError(s"Expected ${tpe1}, but got ${tpe2}")

    def unifyBlockTypes(tpe1: FunctionType, tpe2: FunctionType)(implicit C: Context): UnificationResult =
      (tpe1, tpe2) match {
        // TODO also consider type parameters here
        case (f1 @ FunctionType(_, args1, ret1), f2 @ FunctionType(_, args2, ret2)) =>

          if (args1.size != args2.size) {
            return UnificationError(s"Section count does not match $f1 vs. $f2")
          }

          (args1 zip args2).foldLeft(unifyValueTypes(ret1, ret2)) {
            case (u, (as1, as2)) =>
              if (as1.size != as2.size)
                return UnificationError(s"Argument count does not match $f1 vs. $f2")

              (as1 zip as2).foldLeft(u) { case (u, (a1, a2)) => u union unifyTypes(a1, a2) }
          }
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
    def instantiate(tpe: FunctionType)(implicit C: Context): (List[RigidVar], FunctionType) = {
      val FunctionType(tparams, params, ret) = tpe
      val subst = tparams.map { p => p -> RigidVar(p) }.toMap
      val rigids = subst.values.toList

      val substitutedParams = subst.substitute(params)
      val substitutedReturn = subst.substitute(ret)
      (rigids, FunctionType(Nil, substitutedParams, substitutedReturn))
    }
  }
}
