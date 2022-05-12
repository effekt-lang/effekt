package effekt

import effekt.context.Context
import effekt.regions.{ Region, RegionEq }
import effekt.symbols.{ FunctionType, CapabilityType, Effect, Effects, EffectApp, BoxedType, BlockType, RigidVar, Type, TypeApp, TypeVar, ValueType }
import effekt.symbols.builtins.THole
import effekt.util.messages.ErrorReporter

object substitutions {

  type Substitutions = Map[TypeVar, ValueType]

  // Substitution is independent of the unifier
  implicit class SubstitutionOps(substitutions: Map[TypeVar, ValueType]) {
    def substitute(t: ValueType): ValueType = t match {
      case x: TypeVar =>
        substitutions.getOrElse(x, x)
      case TypeApp(t, args) =>
        TypeApp(t, args.map { substitute })
      case BoxedType(tpe, reg) =>
        BoxedType(substitute(tpe), reg)
      case other => other
    }

    def substitute(e: Effects): Effects = Effects(e.toList.map(substitute))

    def substitute(e: Effect): Effect = e match {
      case EffectApp(e, tpes) => EffectApp(substitute(e), tpes.map(substitute))
      case e                  => e
    }

    def substitute(t: BlockType): BlockType = t match {
      case b: CapabilityType => b
      case b: FunctionType   => substitute(b)
    }

    def substitute(t: FunctionType): FunctionType = t match {
      case FunctionType(tps, vps, bps, ret, effs) =>
        val substWithout = substitutions.filterNot { case (t, _) => tps.contains(t) }
        FunctionType(tps, vps map substWithout.substitute, bps map substWithout.substitute, substWithout.substitute(ret), substWithout.substitute(effs))
    }
  }

  sealed trait UnificationResult {
    def add(k: TypeVar, v: ValueType): UnificationResult
    def union(other: UnificationResult): UnificationResult
    def checkFullyDefined(rigids: List[RigidVar]): UnificationResult
    def getUnifier(implicit error: ErrorReporter): Unifier
    def equalRegions(r1: Region, r2: Region)(implicit C: Context): UnificationResult
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

    def equalRegions(r1: Region, r2: Region)(implicit C: Context): Unifier =
      this.copy(constraints = constraints + RegionEq(r1, r2, C.focus))

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
    def equalRegions(r1: Region, r2: Region)(implicit C: Context) = this
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

        case (t: FunctionType, s: FunctionType) =>
          unifyFunctionTypes(t, s)

        case (t, s) =>
          UnificationError(s"Expected ${t}, but got ${s}")
      }

    def unifyValueTypes(tpe1: ValueType, tpe2: ValueType)(implicit C: Context): UnificationResult =
      (tpe1.dealias, tpe2.dealias) match {

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

        case (BoxedType(tpe1, reg1), BoxedType(tpe2, reg2)) =>
          unifyTypes(tpe1, tpe2).equalRegions(reg1, reg2)

        case (t, s) =>
          UnificationError(s"Expected ${t}, but got ${s}")
      }

    def unifyBlockTypes(tpe1: BlockType, tpe2: BlockType)(implicit C: Context): UnificationResult = ???

    def unifyFunctionTypes(tpe1: FunctionType, tpe2: FunctionType)(implicit C: Context): UnificationResult =
      (tpe1, tpe2) match {
        // TODO also consider type parameters here
        case (f1 @ FunctionType(tps1, vps1, bps1, ret1, eff1), f2 @ FunctionType(tps2, vps2, bps2, ret2, eff2)) =>

          if (tps1.size != tps2.size || vps1.size != vps2.size || bps1.size != bps2.size) {
            return UnificationError(s"Argument count does not match $f1 vs. $f2")
          }

          // TODO substitute type parameters
          //   https://github.com/effekt-lang/effekt/blob/31b05ba42df031a325245c30220aa5d9bb33a7ff/effekt/shared/src/main/scala/effekt/Substitution.scala#L477-L498

          // We don't unify effects here, instead we simply gather them
          // Two effects State[?X1] and State[?X2] are assumed to be disjoint until we know that ?X1 and ?X2 are equal.
          var unifier = unifyTypes(ret1, ret2)
          (vps1 zip vps2).foreach { case (tpe1, tpe2) => unifier = unifier union unifyValueTypes(tpe1, tpe2) }
          (bps1 zip bps2).foreach { case (tpe1, tpe2) => unifier = unifier union unifyBlockTypes(tpe1, tpe2) }

          unifier
      }

    /**
     * Instantiate a typescheme with fresh, rigid type variables
     *
     * i.e. `[A, B] (A, A) => B` becomes `(?A, ?A) => ?B`
     */
    def instantiate(tpe: FunctionType)(implicit C: Context): (List[RigidVar], FunctionType) = {
      val FunctionType(tparams, vparams, bparams, ret, effs) = tpe
      val subst = tparams.map { p => p -> RigidVar(p) }.toMap
      val rigids = subst.values.toList

      val substitutedValueParams = vparams map subst.substitute
      val substitutedBlockParams = bparams map subst.substitute

      // here we also replace all region variables by copies to allow
      // substitution in RegionChecker.
      val substitutedReturn = subst.substitute(freshRegions(ret))
      val substitutedEffects = subst.substitute(freshRegions(effs))
      (rigids, FunctionType(Nil, substitutedValueParams, substitutedBlockParams, substitutedReturn, substitutedEffects))
    }

    /**
     * Replaces all regions with fresh region variables and instantiates
     * them with the underlying variable.
     */
    def freshRegions[T](t: T)(implicit C: Context): T = {

      def generic[T](t: T): T = t match {
        case t: ValueType    => visitValueType(t).asInstanceOf[T]
        case b: FunctionType => visitFunctionType(b).asInstanceOf[T]
        case e: Effects      => visitEffects(e).asInstanceOf[T]
        case other           => C.panic(s"Don't know how to traverse ${t}")
      }

      def visitValueType(t: ValueType): ValueType = t match {
        case x: TypeVar => x
        case TypeApp(t, args) =>
          TypeApp(t, args.map { visitValueType })
        case BoxedType(tpe, reg) =>
          // this is the **only** interesting case...
          // vvvvvvvvvvvvvvvvvvvvvvvvvvvvv
          val copy = Region.fresh(C.focus)
          copy.instantiate(reg)
          // ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
          BoxedType(visitFunctionType(tpe), copy)
        case other => other
      }

      def visitEffects(effs: Effects): Effects = Effects(effs.toList.map(visitEffect))

      def visitEffect(e: Effect): Effect = e match {
        // TODO do we need to dealias here?
        case EffectApp(eff, targs) => EffectApp(eff, targs map visitValueType)
        case e                     => e
      }

      def visitBlockType(t: BlockType): BlockType = t match {
        case f: FunctionType   => visitFunctionType(f)
        case c: CapabilityType => c // TODO cover case
      }

      def visitFunctionType(t: FunctionType): FunctionType = t match {
        case FunctionType(tps, vps, bps, ret, eff) =>
          FunctionType(tps, vps map visitValueType, bps map visitBlockType, visitValueType(ret), visitEffects(eff))
      }

      generic(t)
    }
  }
}
