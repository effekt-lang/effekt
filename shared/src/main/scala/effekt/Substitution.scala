package effekt

import effekt.context.Context
import effekt.regions.{ Region, RegionEq }
import effekt.symbols.{ BlockType, CapabilityType, Effectful, FunType, InterfaceType, RigidVar, Sections, Type, TypeApp, TypeVar, ValueType }
import effekt.symbols.builtins.THole
import effekt.util.messages.ErrorReporter

object subtitutions {

  type Substitutions = Map[TypeVar, ValueType]

  sealed trait UnificationResult {
    def add(k: TypeVar, v: ValueType): UnificationResult
    def union(other: UnificationResult): UnificationResult
    def checkFullyDefined(rigids: List[RigidVar]): UnificationResult
    def getUnifier(implicit error: ErrorReporter): Unifier
    def equalRegions(r1: Region, r2: Region): UnificationResult
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
      val newUnifier = Unifier(k -> v)
      val improvedSubst: Substitutions = substitutions.map { case (rigid, tpe) => (rigid, newUnifier substitute tpe) }
      Unifier(improvedSubst + (k -> Unifier(improvedSubst, constraints).substitute(v)), constraints)
    }

    def equalRegions(r1: Region, r2: Region): Unifier =
      this.copy(constraints = constraints + RegionEq(r1, r2))

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

    def substitute(t: Type): Type = t match {
      case t: ValueType      => substitute(t)
      case b: BlockType      => substitute(b)
      case b: CapabilityType => b
    }

    def substitute(t: ValueType): ValueType = t match {
      case x: TypeVar =>
        substitutions.getOrElse(x, x)
      case TypeApp(t, args) =>
        TypeApp(t, args.map { substitute })
      case FunType(tpe, reg) =>
        FunType(substitute(tpe), reg)
      case other => other
    }

    def substitute(e: Effectful): Effectful = e match {
      case Effectful(tpe, effs) => Effectful(substitute(tpe), effs)
    }

    def substitute(t: InterfaceType): InterfaceType = t match {
      case b: CapabilityType => b
      case b: BlockType      => substitute(b)
    }

    def substitute(t: BlockType): BlockType = t match {
      case BlockType(tps, ps, ret) =>
        val substWithout = Unifier(substitutions.filterNot { case (t, _) => ps.contains(t) }, constraints)
        BlockType(tps, substWithout.substitute(ps), substWithout.substitute(ret))
    }

    def substitute(t: Sections): Sections = t map {
      _ map {
        case v: ValueType     => substitute(v)
        case b: InterfaceType => substitute(b)
      }
    }
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
    def equalRegions(r1: Region, r2: Region) = this
  }

  object Unification {

    // just for backwards compat
    def unify(tpe1: Type, tpe2: Type)(implicit C: ErrorReporter): Unifier =
      unifyTypes(tpe1, tpe2).getUnifier

    // The lhs can contain rigid vars that we can compute a mapping for
    // i.e. unify(List[?A], List[Int]) = Map(?A -> Int)
    def unifyTypes(tpe1: Type, tpe2: Type): UnificationResult =
      (tpe1, tpe2) match {

        case (t: ValueType, s: ValueType) =>
          unifyValueTypes(t, s)

        // TODO also consider type parameters here
        case (f1 @ BlockType(_, args1, ret1), f2 @ BlockType(_, args2, ret2)) =>

          if (args1.size != args2.size) {
            return UnificationError(s"Section count does not match $f1 vs. $f2")
          }

          (args1 zip args2).foldLeft(unifyTypes(ret1.tpe, ret2.tpe)) {
            case (u, (as1, as2)) =>
              if (as1.size != as2.size)
                return UnificationError(s"Argument count does not match $f1 vs. $f2")

              (as1 zip as2).foldLeft(u) { case (u, (a1, a2)) => u union unifyTypes(a1, a2) }
          }

        case (t, s) =>
          UnificationError(s"Expected ${t}, but got ${s}")
      }

    def unifyValueTypes(tpe1: ValueType, tpe2: ValueType): UnificationResult =
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

        case (FunType(tpe1, reg1), FunType(tpe2, reg2)) =>
          unifyTypes(tpe1, tpe2).equalRegions(reg1, reg2)

        case (t, s) =>
          UnificationError(s"Expected ${tpe1}, but got ${tpe2}")
      }

    /**
     * Instantiate a typescheme with fresh, rigid type variables
     *
     * i.e. `[A, B] (A, A) => B` becomes `(?A, ?A) => ?B`
     */
    def instantiate(tpe: BlockType)(implicit C: Context): (List[RigidVar], BlockType) = {
      val BlockType(tparams, params, ret) = tpe
      val subst = tparams.map { p => p -> RigidVar(p) }.toMap
      val unifier = Unifier(subst)
      val rigids = subst.values.toList

      val substitutedParams = unifier.substitute(params)

      // here we also replace all region variables by copies to allow
      // substitution in RegionChecker.
      val substitutedReturn = unifier.substitute(freshRegions(ret))
      (rigids, BlockType(Nil, substitutedParams, substitutedReturn))
    }

    /**
     * Replaces all regions with fresh region variables and instantiates
     * them with the underlying variable.
     */
    def freshRegions[T](t: T)(implicit C: Context): T = {

      def generic[T](t: T): T = t match {
        case t: ValueType => visitValueType(t).asInstanceOf[T]
        case b: BlockType => visitBlockType(b).asInstanceOf[T]
        case b: Effectful => visitEffectful(b).asInstanceOf[T]
        case s: List[List[Type] @unchecked] => visitSections(s).asInstanceOf[T]
        case other => C.panic(s"Don't know how to traverse ${t}")
      }

      def visitValueType(t: ValueType): ValueType = t match {
        case x: TypeVar => x
        case TypeApp(t, args) =>
          TypeApp(t, args.map { visitValueType })
        case FunType(tpe, reg) =>
          // this is the **only** interesting case...
          // vvvvvvvvvvvvvvvvvvvvvvvvvvvvv
          val copy = Region.fresh(C.focus)
          copy.instantiate(reg)
          // ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
          FunType(visitBlockType(tpe), copy)
        case other => other
      }

      def visitEffectful(e: Effectful): Effectful = e match {
        case Effectful(tpe, effs) => Effectful(visitValueType(tpe), effs)
      }

      def visitBlockType(t: BlockType): BlockType = t match {
        case BlockType(tps, ps, ret) =>
          BlockType(tps, visitSections(ps), visitEffectful(ret))
      }

      def visitSections(t: Sections): Sections = t map {
        _ map {
          case v: ValueType      => visitValueType(v)
          case b: BlockType      => visitBlockType(b)
          case b: CapabilityType => b
        }
      }
      generic(t)
    }
  }
}
